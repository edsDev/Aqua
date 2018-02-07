module Aqua.TypeChecker

open Aqua.Language
open Aqua.Syntax
open Aqua.Ast
open Aqua.ErrorMessage
open Aqua.Compiler

open Aqua.Compiler.TranslationEnvironment
open Aqua.Compiler.TranslationContext
open Aqua.Parser.ParsecInstance

let existImplicitConversion env srcType destType =
    match srcType, destType with
    | SystemStub(t1), SystemStub(t2)    -> t1=t2
    | _                                 -> false

let isInvocable env signature argTypeList =
    let (FunctionSignature(paramTypeList, _)) = signature

    List.length paramTypeList = List.length argTypeList &&
    List.forall2 (existImplicitConversion env) argTypeList paramTypeList

let isAssignable env expr =
    match expr with
    | Ast_NameAccessExpr(_, mut, _) -> mut=Mutable
    | _                             -> false

let resolveFunctionCall env rg typeName funcName argTypeList =
    // TODO: refine overload resolving
    let functions = 
        lookupCallableName env "" funcName
        |> List.filter (fun def -> isInvocable env def.Signature argTypeList)

    match functions with
    | [x] ->
        Ok x
    | [] ->
        Error (invalidFunctionCall rg funcName argTypeList)
    | _ ->
        Error (invalidFunctionResolve rg funcName argTypeList)

let resolveExpressionCall env rg calleeExpr argTypeList =
    match exprType with
    | FunctionStub(signature) when isInvocable env signature argTypeList ->
        Ok signature
    | _ ->
        Error (invalidExpressionCall rg argTypeList)

type SyntaxTypeChecker = 
    TranslationContext -> SyntaxType -> TranslationContext
type ExpressionTranslator = 
    TranslationContext -> SyntaxExpr -> AstExpr option*TranslationContext
type StatementChecker = 
    TranslationContext -> SyntaxStmt -> TranslationContext

let rec translateType env ctx type' =
    match type' with
    | Syn_SystemType(_, category) -> 
        ctx |> makeEvalResult (SystemStub(category))

    | Syn_UserType(rg, name) ->
        match lookupType env name with
        | Some _ -> ctx |> makeEvalResult (UserStub(name)) 
        | None -> ctx |> makeEvalError (invalidUserType rg name)

    | Syn_FunctionType(_, paramTypeList, retType) ->
        retType::paramTypeList
        |> List.mapFold (translateType env) ctx
        |> bindEvalResultList 
               (fun ts ->
                    let paramsStub = ts |> List.tail
                    let returnStub = ts |> List.head
                    
                    Ok <| makeFunctionStub paramsStub returnStub)

let rec translateExpr env ctx expr =

    // shortcuts for ease of recursive invocation
    //
    let translateTypeAux = translateType env
    let translateExprAux = translateExpr env

    let translateExprTypePair ctx e t =
        let e', ctx1 = translateExprAux ctx e
        let t', ctx2 = translateTypeAux ctx1 t

        e', t', ctx2

    let translateExprExprPair ctx e1 e2 =
        let e1', ctx1 = translateExprAux ctx e1
        let e2', ctx2 = translateExprAux ctx1 e2

        e1', e2', ctx2

    let translateExprList originalCtx exprList =
        List.mapFold translateExprAux originalCtx exprList

    // case-wise translators
    //
    let translateLiteralExpr rg value =
        ctx |> makeEvalResult (Ast_LiteralExpr value)

    let translateNameAccessExpr rg name =
        match lookupVariable ctx name with
        | Some(VariableLookupItem(_, mut, t)) -> 
            ctx |> makeEvalResult (Ast_NameAccessExpr(t, mut, VariableLocal name))
        | None ->
            ctx |> makeEvalError (invalidVariableReference rg name)

    let translateMemberAccessExpr rg child name =
        translateExprAux ctx child
        |> bindEvalResult (fun e -> Ok <| Ast_MemberAccessExpr(kUnitType, e, name))

    let translateTypeCheckExpr rg child testType =
        translateExprTypePair ctx child testType
        |> bindEvalResult2 (fun e t -> Ok <| Ast_TypeCheckExpr(e, t))

    let translateTypeCastExpr rg child targetType =
        translateExprTypePair ctx child targetType
        |> bindEvalResult2 (fun e t -> Ok <| Ast_TypeCastExpr(e, t))

    let translateInvocationExpr rg calleeExpr args =
        let args', newCtx = translateExprList ctx args

        if args' |> List.forall Option.isSome then
            let argValueList = args' |> List.map Option.get
            let argTypeList = argValueList |> List.map AstExpr.getTypeStub

            match calleeExpr with
            | Syn_NameAccessExpr(nameRange, funcName) when lookupVariable ctx funcName |> Option.isNone ->
                match resolveFunctionCall env nameRange "" funcName argTypeList with
                | Ok f ->
                    newCtx |> makeEvalResult (Ast_InvocationExpr(CallableFunction f, argValueList))
                | Error msg ->
                    newCtx |> makeEvalError msg
            | Syn_MemberAccessExpr(nameRange, srcExpr, methodName) when true ->
                translateExprAux newCtx srcExpr
                |> bindEvalResult
                       (fun e ->
                            let typeName = (AstExpr.getTypeStub e).ToString()

                            resolveFunctionCall env nameRange typeName methodName argTypeList
                            |> Result.map (fun f -> Ast_InvocationExpr(CallableMethod(typeName, f), argValueList))
            | _ ->
                translateExprAux newCtx calleeExpr
                |> bindEvalResult 
                       (fun e ->
                              match AstExpr.getTypeStub e with
                              | FunctionStub(s) when isInvocable env s argTypeList ->
                                   Ok <| Ast_InvocationExpr(CallableExpression(e, s), argValueList)
                              | _ ->
                                   Error <| invalidExpressionCall calleeExpr.Range argTypeList)
                
        else
            newCtx |> makeEvalErrorEscape

    let translateBinaryExpr rg op lhs rhs =
        translateExprExprPair ctx lhs rhs
        |> bindEvalResult2
               (fun e1 e2 ->
                    let lhsType = AstExpr.getTypeStub e1
                    let rhsType = AstExpr.getTypeStub e2

                    match op with
                        | Op_Assign ->
                            if not <| isAssignable env e1 then
                                Error <| invalidAssignment lhs.Range
                            else if not <| existImplicitConversion env rhsType lhsType then
                                Error <| invalidBinaryOperation rg op lhsType rhsType
                            else
                                // TODO: make assignment an independent case?
                                Ok <| Ast_BinaryExpr(lhsType, op, e1, e2)

                        | Op_Plus | Op_Minus
                        | Op_Asterisk | Op_Slash
                        | Op_Modulus 
                        | Op_BitwiseAnd | Op_BitwiseOr
                        | Op_BitwiseXor ->
                            match lhsType, rhsType with
                            | SystemStub(Int), SystemStub(Int) ->
                                Ok <| Ast_BinaryExpr(kIntType, op, e1, e2)
                            | SystemStub(_), SystemStub(_) ->
                                Error <| invalidBinaryOperation rg op lhsType rhsType
                            | _ -> 
                                failwith "user type not supported yet"

                        | Op_Equal | Op_NotEqual
                        | Op_Greater | Op_GreaterEq
                        | Op_Less | Op_LessEq ->
                            match lhsType, rhsType with
                            | SystemStub(x), SystemStub(y) when x=y && x<>Unit ->
                                Ok <| Ast_BinaryExpr(kBoolType, op, e1, e2)
                            | SystemStub(_), SystemStub(_) ->
                                Error <| invalidBinaryOperation rg op lhsType rhsType
                            | _ -> 
                                failwith "user type not supported yet"

                        | Op_Conjunction | Op_Disjunction ->
                            match lhsType, rhsType with
                            | SystemStub(Bool), SystemStub(Bool) ->
                                Ok <| Ast_BinaryExpr(kBoolType, op, e1, e2)

                            | SystemStub(_), SystemStub(_) ->
                                Error <| invalidBinaryOperation rg op lhsType rhsType

                            | _ -> 
                                failwith "user type not supported yet")

    //
    //
    match expr with
    | Syn_LiteralExpr(rg, value)                -> translateLiteralExpr rg value
    | Syn_NameAccessExpr(rg, name)              -> translateNameAccessExpr rg name
    | Syn_MemberAccessExpr(rg, child, name)     -> translateMemberAccessExpr rg child name
    | Syn_TypeCheckExpr(rg, child, testType)    -> translateTypeCheckExpr rg child testType
    | Syn_TypeCastExpr(rg, child, targetType)   -> translateTypeCastExpr rg child targetType
    | Syn_InvocationExpr(rg, callee, args)      -> translateInvocationExpr rg callee args
    | Syn_BinaryExpr(rg, op, lhs, rhs)          -> translateBinaryExpr rg op lhs rhs

let rec translateStmt env ctx stmt =
    
    // shortcuts for ease of recursive invocation
    //
    let translateTypeAux = translateType env
    let translateExprAux = translateExpr env
    let translateStmtAux = translateStmt env

    // auxiliary functions
    //
    let ensureTypeConversion rg srcType destType =
        if existImplicitConversion env srcType destType then
            withIdentity
        else
            appendError (invalidImpilicitConversion rg srcType destType)

    let ensureExprType expr constraintType ctx =
        translateExprAux ctx expr
        |> bindEvalResult
               (fun e ->
                    let exprType = e |> AstExpr.getTypeStub
                    if existImplicitConversion env exprType constraintType then
                        Ok <| e
                    else
                        Error <| invalidImpilicitConversion expr.Range exprType constraintType)

    // case-wise translators
    //
    let translateExpressionStmt rg expr =
        translateExprAux ctx expr
        |> bindEvalResult
               (function
                | Ast_BinaryExpr(_, Op_Assign, _, _)
                | Ast_InvocationExpr(_, _) as x ->
                    Ok <| Ast_ExpressionStmt x
                | _ ->
                    Error <| invalidExpressionStmt rg)

    let translateVarDeclStmt rg mut name typeAnnot init =
        let init', newCtx = translateExprAux ctx init
        

        let newContext, exprType = withLastExprType <| translateExprAux ctx init
        let checker =
            match lookupVariable newContext name with
            | Some _ ->
                appendError (invalidVariableDecl rg name)
            | None ->
                match typeAnnot with
                | Some t ->
                    ensureTypeConversion rg exprType t.Stub
                    >> declareVariable name mut t.Stub
                | None -> 
                    declareVariable name mut exprType

        checker <| newContext

    let translateChoiceStmt rg pred pBranch nBranch =
        let newContext = ensureExprType pred kBoolType ctx
        
        [ Some pBranch; nBranch; ]
        |> List.choose id
        |> List.mapFold translateStmtAux newContext

    let translateWhileStmt rg pred body =
        ensureExprType pred kBoolType ctx
        |> enterLoopBody
        |> (fun ctx -> translateStmtAux ctx body)
        |> exitLoopBody

    let translateControlFlowStmt rg ctrl =
        let checker =
            match testLoopBody ctx with
            | true -> withIdentity
            | false -> appendError (invalidControlFlow rg ctrl)

        checker <| ctx

    let translateReturnStmt rg maybeExpr =
        let newContext, exprType =
            match maybeExpr with
            | Some(expr) -> withLastExprType <| translateExprAux ctx expr
            | None       -> ctx, kUnitType

        ensureTypeConversion rg exprType (ctx.ReturnType.Stub) newContext

    let translateCompoundStmt rg children =
        List.mapFold translateStmtAux ctx children
        |> bindEvalResultList (Ok << Ast_CompoundStmt)
        |> updateContext (restoreScope ctx)

    //
    //
    match stmt with
    | Syn_ExpressionStmt(rg, expr) -> translateExpressionStmt rg expr
    | Syn_VarDeclStmt(rg, mut, name, typeAnnot, init) -> translateVarDeclStmt rg mut name typeAnnot init
    | Syn_ChoiceStmt(rg, pred, pBranch, nBranch) -> translateChoiceStmt rg pred pBranch nBranch
    | Syn_WhileStmt(rg, pred, body) -> translateWhileStmt rg pred body
    | Syn_ControlFlowStmt(rg, ctrl) -> translateControlFlowStmt rg ctrl
    | Syn_ReturnStmt(rg, maybeExpr) -> translateReturnStmt rg maybeExpr
    | Syn_CompoundStmt(rg, children) -> translateCompoundStmt rg children

let checkFunction env func =
    let ctx = translateStmt env (TranslationContext.createContext func) (func.Body)
    ctx
    //{ ExprTypeLookup = ctx.ExprTypeCache }