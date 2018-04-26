module Aqua.TypeChecker

open Aqua.Language
open Aqua.Syntax
open Aqua.Ast
open Aqua.ErrorMessage
open Aqua.Compiler
open Aqua.CollectionUtils

let existImplicitConversion ctx srcType destType =
    srcType=destType

let isInvocable ctx signature argTypeList =
    let (FunctionSignature(paramTypeList, _)) = signature

    List.length paramTypeList = List.length argTypeList &&
    List.forall2 (existImplicitConversion ctx) argTypeList paramTypeList

let isAssignable ctx expr =
    match expr with
    | Ast_NameAccessExpr(_, mut, _) ->
        mut = MutabilitySpec.Mutable
    | Ast_MemberAccessExpr(_, e, name) ->
        let fieldOwner = (getAstExprType e).ToString()

        match lookupField ctx fieldOwner name with
        | Some def -> def.Mutability = MutabilitySpec.Mutable
        | None -> false
    | _ ->
        false

let resolveImplicitMethodCall ctx rg funcName argTypeList =
    []

let resolveExplicitMethodCall ctx rg selfExpr funcName argTypeList =
    []

let resolveMethodCall ctx rg typeName methodName argList =
    let argTypeList =
        argList |> List.map getAstExprType

    let candidates =
        lookupMethod ctx typeName methodName
        |> List.filter (fun def -> isInvocable ctx def.Signature argTypeList)

    match candidates with
    | [x] ->
        Ok <| Ast_InvocationExpr(CallableMethod(typeName, x), argList)
    | [] ->
        Error <| invalidFunctionCall rg methodName argTypeList
    | _ ->
        Error <| invalidFunctionResolve rg methodName argTypeList


let resolveExpressionCall ctx rg calleeExpr argList =
    let argTypeList =
        argList |> List.map getAstExprType

    match calleeExpr |> getAstExprType with
    | FunctionTypeIdent(signature) when isInvocable ctx signature argTypeList ->
        Ok <| Ast_InvocationExpr(CallableExpression(calleeExpr, signature), argList)
    | _ ->
        Error <| invalidExpressionCall rg argTypeList

type SyntaxTypeTranslator =
    TranslationContext -> SyntaxType -> TypeIdent option*TranslationContext
type ExpressionTranslator =
    TranslationContext -> SyntaxExpr -> AstExpr option*TranslationContext
type StatementTranslator =
    TranslationContext -> SyntaxStmt -> AstStmt option*TranslationContext

let translatePair (f, g) ctx x y =
    let x', ctx1 = f ctx x
    let y', ctx2 = g ctx1 y

    x', y', ctx2

let translateTriple (f, g, h) ctx x y z =
    let x', ctx1 = f ctx x
    let y', ctx2 = g ctx1 y
    let z', ctx3 = h ctx2 z

    x', y', z', ctx3

// helpers
//

let rec translateType ctx type' =
    match type' with
    | Syn_SystemType(_, category) ->
        ctx |> makeEvalResult (SystemTypeIdent category)

    | Syn_UserType(rg, name) ->
        match lookupType ctx name with
        | Some item -> ctx |> makeEvalResult item.Type
        | None -> ctx |> makeEvalError (invalidUserType rg name)

    | Syn_FunctionType(_, paramTypeList, retType) ->
        retType::paramTypeList
        |> List.mapFold translateType ctx
        |> processEvalResultList
               (fun ts ->
                    let paramsStub = ts |> List.tail
                    let returnStub = ts |> List.head

                    Ok <| makeFunctionTypeIdent paramsStub returnStub)

let rec translateExpr ctx expr =

    // case-wise translate functions
    //
    let translateInstanceExpr rg =
        if isInstanceContext ctx then
            let t = (getCurrentKlassType ctx)
            ctx |> makeEvalResult (Ast_InstanceExpr t)
        else
            ctx |> makeEvalError (invalidInstanceReference rg)

    let translateLiteralExpr rg value =
        ctx |> makeEvalResult (Ast_LiteralExpr value)

    let translateNameAccessExpr rg name =
        match lookupVariable ctx name with
        | Some (ArgumentLookupItem(id, _, t)) ->
            ctx |> makeEvalResult (Ast_NameAccessExpr(t, MutabilitySpec.Readonly, VariableArgument id))
        | Some (VariableLookupItem(id, _, t, mut)) ->
            ctx |> makeEvalResult (Ast_NameAccessExpr(t, mut, VariableLocal id))
        | None ->
            ctx |> makeEvalError (invalidVariableReference rg name)

    let translateMemberAccessExpr rg child name =
        translateExpr ctx child
        |> processEvalResult
               (fun e ->
                    let t = getAstExprType e
                    let typeName = getTypeName t
                    match lookupField ctx typeName name with
                    | Some x ->
                        Ok <| Ast_MemberAccessExpr(x.Type, e, name)
                    | None ->
                        Error <| invalidFieldReference rg typeName name)

    let translateTypeCheckExpr rg child testType =
        translatePair (translateExpr, translateType) ctx child testType
        |> processEvalResult2 (fun e t -> Ok <| Ast_TypeCheckExpr(e, t))

    let translateTypeCastExpr rg child targetType =
        translatePair (translateExpr, translateType) ctx child targetType
        |> processEvalResult2 (fun e t -> Ok <| Ast_TypeCastExpr(e, t))

    let translateInvocationExpr rg calleeExpr args =

        // try translate arguments provided
        let maybeAstArgs, newCtx = List.mapFold translateExpr ctx args

        if maybeAstArgs |> List.forall Option.isSome then
            let astArgs = maybeAstArgs |> List.map Option.get

            match calleeExpr with
            | Syn_MemberAccessExpr(nameRange, srcExpr, methodName) when true ->
                translateExpr newCtx srcExpr
                |> processEvalResult
                       (fun e ->
                            let typeName = (getAstExprType e).ToString()
                            resolveMethodCall ctx nameRange typeName methodName astArgs)
            | _ ->
                translateExpr newCtx calleeExpr
                |> processEvalResult
                       (fun e ->
                            resolveExpressionCall ctx calleeExpr.Range e astArgs)

        else
            newCtx |> escapeEvalError

    let translateBinaryExpr rg op lhs rhs =
        translatePair (translateExpr, translateExpr) ctx lhs rhs
        |> processEvalResult2
               (fun e1 e2 ->
                    let lhsType = getAstExprType e1
                    let rhsType = getAstExprType e2

                    match op with
                    | Op_Assign ->
                        if not <| isAssignable ctx e1 then
                            Error <| invalidAssignment lhs.Range
                        else if not <| existImplicitConversion ctx rhsType lhsType then
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
                        | SystemTypeIdent(Int), SystemTypeIdent(Int) ->
                            Ok <| Ast_BinaryExpr(kIntType, op, e1, e2)
                        | SystemTypeIdent(_), SystemTypeIdent(_) ->
                            Error <| invalidBinaryOperation rg op lhsType rhsType
                        | _ ->
                            failwith "user type not supported yet"

                    | Op_Equal | Op_NotEqual
                    | Op_Greater | Op_GreaterEq
                    | Op_Less | Op_LessEq ->
                        match lhsType, rhsType with
                        | SystemTypeIdent(x), SystemTypeIdent(y) when x=y && x<>Unit ->
                            Ok <| Ast_BinaryExpr(kBoolType, op, e1, e2)
                        | SystemTypeIdent(_), SystemTypeIdent(_) ->
                            Error <| invalidBinaryOperation rg op lhsType rhsType
                        | _ ->
                            failwith "user type not supported yet"

                    | Op_Conjunction | Op_Disjunction ->
                        match lhsType, rhsType with
                        | SystemTypeIdent(Bool), SystemTypeIdent(Bool) ->
                            Ok <| Ast_BinaryExpr(kBoolType, op, e1, e2)

                        | SystemTypeIdent(_), SystemTypeIdent(_) ->
                            Error <| invalidBinaryOperation rg op lhsType rhsType

                        | _ ->
                            failwith "user type not supported yet")

    //
    //
    match expr with
    | Syn_InstanceExpr(rg)                      -> translateInstanceExpr rg
    | Syn_LiteralExpr(rg, value)                -> translateLiteralExpr rg value
    | Syn_NameAccessExpr(rg, name)              -> translateNameAccessExpr rg name
    | Syn_MemberAccessExpr(rg, child, name)     -> translateMemberAccessExpr rg child name
    | Syn_TypeCheckExpr(rg, child, testType)    -> translateTypeCheckExpr rg child testType
    | Syn_TypeCastExpr(rg, child, targetType)   -> translateTypeCastExpr rg child targetType
    | Syn_InvocationExpr(rg, callee, args)      -> translateInvocationExpr rg callee args
    | Syn_BinaryExpr(rg, op, lhs, rhs)          -> translateBinaryExpr rg op lhs rhs

let rec translateStmt ctx stmt =

    // auxiliary functions
    //

    let ensureExprType expr constraintType ctx =
        translateExpr ctx expr
        |> processEvalResult
               (fun e ->
                    let exprType = e |> getAstExprType
                    if existImplicitConversion ctx exprType constraintType then
                        Ok <| e
                    else
                        Error <| invalidImpilicitConversion expr.Range exprType constraintType)

    // case-wise translators
    //
    let translateExpressionStmt rg expr =
        translateExpr ctx expr
        |> processEvalResult
               (function
                | Ast_BinaryExpr(_, Op_Assign, _, _)
                | Ast_InvocationExpr(_, _) as x ->
                    Ok <| Ast_ExpressionStmt x
                | _ ->
                    Error <| invalidExpressionStmt rg)


    let translateVarDeclStmt rg mut name typeAnnot init =
        match lookupVariable ctx name with
        | Some _ ->
            ctx |> makeEvalError (invalidVariableDecl rg name)
        | None ->
            translateExpr ctx init
            |> bindEvalResult
                   (fun newCtx value ->
                        let initType = getAstExprType value
                        let id = getNextVarId newCtx

                        match typeAnnot with
                        | Some t ->
                            translateType newCtx t
                            |> bindEvalResult
                                   (fun newCtx2 declType ->
                                        declareVariable name mut declType newCtx2
                                        |> if existImplicitConversion newCtx initType declType
                                           then makeEvalResult (Ast_VarDeclStmt(id, declType, value))
                                           else makeEvalError (invalidImpilicitConversion rg initType declType))
                        | None ->
                            declareVariable name mut initType newCtx
                            |> makeEvalResult (Ast_VarDeclStmt(id, initType, value)))

    let translateChoiceStmt rg pred pBranch nBranch =
        ensureExprType pred kBoolType ctx
        |> bindEvalResult
               (fun newCtx predExpr ->
                    let pBody = pBranch
                    match nBranch with
                    | Some nBody ->
                        translatePair (translateStmt, translateStmt) newCtx pBody nBody
                        |> processEvalResult2 (fun sp sn -> Ok <| Ast_ChoiceStmt(predExpr, sp, Some sn))
                    | None ->
                        translateStmt newCtx pBody
                        |> processEvalResult (fun sp -> Ok <| Ast_ChoiceStmt(predExpr, sp, None)))

    let translateWhileStmt rg pred body =
        ensureExprType pred kBoolType ctx
        |> updateContext enterLoopBody
        |> bindEvalResult
               (fun newCtx predExpr ->
                    translateStmt newCtx body
                    |> processEvalResult (fun s -> Ok <| Ast_WhileStmt(predExpr, s)))
        |> updateContext exitLoopBody

    let translateControlFlowStmt rg ctrl =
        match insideLoopBody ctx with
        | true  -> ctx |> makeEvalResult (Ast_ControlFlowStmt ctrl)
        | false -> ctx |> makeEvalError (invalidControlFlow rg ctrl)

    let translateReturnStmt rg maybeExpr =
        match maybeExpr with
        | Some retValue ->
            ensureExprType retValue (getReturnType ctx) ctx
            |> processEvalResult (fun e -> Ok <| Ast_ReturnStmt (Some e))
        | None ->
            let destType = getReturnType ctx
            if destType=kUnitType then
                ctx |> makeEvalResult (Ast_ReturnStmt None)
            else
                ctx |> makeEvalError (invalidImpilicitConversion rg kUnitType destType)

    let translateCompoundStmt rg children =
        List.mapFold translateStmt ctx children
        |> processEvalResultList (Ok << Ast_CompoundStmt)
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

let translateMethod env body =
    let result, ctx = translateStmt (createContext env) body

    match result with
    | Some s ->
        Ok <| AstMethod(env.CurrentMethod, ctx.VariableList, s)
    | None ->
        Error <| ctx.ErrorMessages

let createTypeLookup session =
    let def2Lookup moduleIdent (klassDef: KlassDefinition) =
        let fieldLookup = DictView.ofSeq <| seq {
            for field in klassDef.Fields do
                yield field.Name, field
        }
        let methodLookup = DictView.ofSeq <| seq {
            for method in klassDef.Methods do
                yield method.Name, List.singleton method
        }

        KlassLookupItem(moduleIdent, klassDef, fieldLookup, methodLookup)

    DictView.ofSeq <| seq {
        for klass in session.CurrentModule.KlassList do
            yield klass.Name, def2Lookup session.CurrentModule.ModuleName klass
    }

let translateModule session =
    let errorBuffer = ResizeArray()
    let typeLookup = createTypeLookup session

    let klass = Seq.toList <| seq {
        for PendingKlass(klassDef, methodList) in session.PendingKlassList do
            let methods = Seq.toList <| seq {
                for PendingMethod(methodDef, body) in methodList do
                    let env = { CurrentModule = session.CurrentModule.ModuleName
                                CurrentKlass = klassDef
                                CurrentMethod = methodDef
                                TypeLookup = typeLookup }

                    match translateMethod env body with
                    | Ok result -> yield result
                    | Error msgs -> errorBuffer.AddRange(msgs)
            }

            yield AstKlass(klassDef, methods)
        }
    
    if errorBuffer.Count = 0 then
        Ok <| klass
    else
        Error <| TranslationError errorBuffer
