module Aqua.TypeChecker

open Aqua.Language
open Aqua.Syntax
open Aqua.ErrorMessage
open Aqua.Compiler

open Aqua.Compiler.TranslationEnvironment
open Aqua.Compiler.TranslationContext
open System.Runtime.InteropServices

let (|UnaryWildcard|_|) t =
    match t with
    | WildcardStub -> Some ()
    | _ -> None

let (|BinaryWildcard|_|) ts =
    match ts with
    | WildcardStub, _ 
    | _, WildcardStub -> Some()
    | _ -> None

let existImplicitConversion env srcType destType =
    match srcType, destType with
    | WildcardStub, _                   -> true
    | _, WildcardStub                   -> true
    | SystemStub(t1), SystemStub(t2)    -> t1=t2
    | _                                 -> false

let isInvocable env signature argTypeList =
    let (FunctionSignature(paramTypeList, _)) = signature

    List.length paramTypeList = List.length argTypeList &&
    List.forall2 (existImplicitConversion env) argTypeList paramTypeList

let isAssignable env varLookup expr =
    match expr with
    | NamedExpr(_, name) -> 
        match varLookup name with
        | Some(VariableLookupItem(_, Mutable, _)) -> true
        | _ -> false

    | _ -> false

let resolveFunctionCall env rg typeName funcName argTypeList =
    // TODO: refine overload resolving
    let functions = 
        lookupFunction env "" funcName
        |> List.filter (fun def -> isInvocable env def.Signature argTypeList)

    match functions with
    | [x] ->
        registerExprType x.Signature.ReturnType
    | [] ->
        appendExprError (invalidFunctionCall rg funcName argTypeList)
    | _ ->
        appendExprError (invalidFunctionResolve rg funcName argTypeList)

let resolveExpressionCall env rg exprType argTypeList =
    match exprType with
    | FunctionStub(signature) when isInvocable env signature argTypeList ->
        registerExprType signature.ReturnType
    | _ ->
        appendExprError (invalidExpressionCall rg argTypeList)

type ExpressionChecker = 
    TranslationContextType -> Expression -> TranslationContextType
type StatementChecker = 
    TranslationContextType -> Statement -> TranslationContextType

let rec checkType env ctx type' =
    ctx

let rec checkExpr env ctx expr =

    // shortcuts for ease of recursive invocation
    //
    let checkExprAux = checkExpr env

    // auxiliary functions
    //

    //
    //
    let newContext, exprChecker =
        match expr with
        | LiteralExpr(_, value) ->
            ctx, LiteralExprChecker env value

        | NamedExpr(rg, name) ->
            ctx, NamedExprChecker env (lookupVariable ctx) rg name

        | TypeCheckExpr(_, child, testType) ->
            let newContext = checkExprAux ctx child
            let checker = TypeCheckExprChecker env testType
        
            newContext, checker

        | TypeCastExpr(_, child, targetType) ->
            let newContext = checkExprAux ctx child
            let checker = TypeCastExprChecker env targetType
        
            newContext, checker

        // 
        | InvocationExpr(_, callee, args) ->
            let ctxOnArgs = args |> List.scan checkExprAux ctx |> List.tail
        
            let newContext = ctxOnArgs |> List.last
            let argTypeList = ctxOnArgs |> List.map (fun tmpCtx -> tmpCtx.LastExprType)

            let checker = InvocationExprChecker env (lookupVariable newContext) callee argTypeList 

            newContext, checker

        | BinaryExpr(rg, op, lhs, rhs) ->
            let newContext, lhsType, rhsType =
                let c1, lt = withLastExprType <| checkExprAux ctx lhs
                let c2, rt = withLastExprType <| checkExprAux c1 rhs

                c2, lt, rt

            let checker = 
                BinaryExprChecker env (lookupVariable newContext) rg op lhs lhsType rhs rhsType

            newContext, checker

    exprChecker expr newContext

and LiteralExprChecker env value =
    match value with
    | BoolConst(_) -> registerExprType kBoolType
    | IntConst(_)  -> registerExprType kIntType

and NamedExprChecker env lookup rg name =
    match lookup name with
    | Some(VariableLookupItem(_, _, t)) -> 
        registerExprType t
    | None ->
        appendExprError (invalidVariableReference rg name)

and TypeCheckExprChecker env testType =
    fun expr ctx ->
        checkType env ctx testType
        |> registerExprType kBoolType expr

and TypeCastExprChecker env targetType =
    // TODO: what if checkType fails?
    fun expr ctx ->
        checkType env ctx targetType
        |> registerExprType targetType.Stub expr

and InvocationExprChecker env varLookup callee argTypeList =
    match callee with
    // call named function
    | NamedExpr(nameRange, funcName) when varLookup funcName |> Option.isNone ->
        resolveFunctionCall env nameRange "" funcName argTypeList

    // call expression delegate
    | _ ->
        fun expr ctx ->
            let currentCtx, fType = 
                withLastExprType <| checkExpr env ctx callee

            resolveExpressionCall env callee.Range fType argTypeList expr currentCtx

and BinaryExprChecker env varLookup rg op lhs lhsType rhs rhsType =
    match op with
    | Op_Assign ->
        if not <| isAssignable env varLookup lhs then
            appendExprError (invalidAssignment lhs.Range)
        else if not <| existImplicitConversion env rhsType lhsType then
            appendExprError (invalidBinaryOperation rg op lhsType rhsType)
        else
            registerExprType lhsType

    | Op_Plus | Op_Minus
    | Op_Asterisk | Op_Slash
    | Op_Modulus 
    | Op_BitwiseAnd | Op_BitwiseOr
    | Op_BitwiseXor ->
        match lhsType, rhsType with
        | BinaryWildcard ->
            registerExprType WildcardStub

        | SystemStub(Int), SystemStub(Int) ->
            registerExprType kIntType

        | SystemStub(_), SystemStub(_) ->
            appendExprError (invalidBinaryOperation rg op lhsType rhsType)

        | _ -> 
            failwith "user type not supported yet"

    | Op_Equal | Op_NotEqual
    | Op_Greater | Op_GreaterEq
    | Op_Less | Op_LessEq ->
        match lhsType, rhsType with
        | BinaryWildcard ->
            registerExprType kBoolType

        | SystemStub(x), SystemStub(y) when x=y && x<>Unit ->
            registerExprType kBoolType

        | SystemStub(_), SystemStub(_) ->
            appendExprError (invalidBinaryOperation rg op lhsType rhsType)

        | _ -> 
            failwith "user type not supported yet"

    | Op_Conjunction | Op_Disjunction ->
        match lhsType, rhsType with
        | BinaryWildcard
        | SystemStub(Bool), SystemStub(Bool) ->
            registerExprType kBoolType

        | SystemStub(_), SystemStub(_) ->
            appendExprError (invalidBinaryOperation rg op lhsType rhsType)

        | _ -> 
            failwith "user type not supported yet"

let rec checkStmt env ctx stmt =
    
    // shortcuts for ease of recursive invocation
    //
    let checkExprAux = checkExpr env
    let checkStmtAux = checkStmt env

    // auxiliary functions
    //
    let ensureTypeConversion rg srcType destType =
        if existImplicitConversion env srcType destType then
            withIdentity
        else
            appendError (invalidImpilicitConversion rg srcType destType)

    let ensureExprType expr constraintType ctx =
        let newContext, exprType = withLastExprType <| checkExprAux ctx expr
        
        ensureTypeConversion expr.Range exprType constraintType newContext

    //
    //
    match stmt with
    | ExpressionStmt(rg, expr) ->
        let checker =
            match expr with
            | BinaryExpr(_, Op_Assign, _, _)
            | InvocationExpr(_, _, _) ->
                withIdentity

            | _ ->
                appendError (invalidExpressionStmt rg)
        
        checker <| checkExprAux ctx expr

    | VarDeclStmt(rg, mut, name, typeAnnot, init) ->
        let newContext, exprType = withLastExprType <| checkExprAux ctx init
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

    | ChoiceStmt(_, pred, posiBranch, negaBranch) ->
        let newContext = ensureExprType pred kBoolType ctx
        
        [ Some posiBranch; negaBranch; ]
        |> List.choose id
        |> List.fold checkStmtAux newContext

    | WhileStmt(_, pred, body) ->
        ensureExprType pred kBoolType ctx
        |> enterLoopBody
        |> (fun ctx -> checkStmtAux ctx body)
        |> exitLoopBody

    | ControlFlowStmt(rg, ctrl) ->
        let checker =
            match testLoopBody ctx with
            | true -> withIdentity
            | false -> appendError (invalidControlFlow rg ctrl)

        checker <| ctx

    | ReturnStmt(rg, maybeExpr) ->
        let newContext, exprType =
            match maybeExpr with
            | Some(expr) -> withLastExprType <| checkExprAux ctx expr
            | None       -> ctx, kUnitType

        ensureTypeConversion rg exprType (ctx.ReturnType.Stub) newContext

    | CompoundStmt(_, children) ->
        List.fold checkStmtAux ctx children |> restoreScope ctx

let checkFunction env func =
    let ctx = checkStmt env (TranslationContext.createContext func) (func.Body)
    ctx
    //{ ExprTypeLookup = ctx.ExprTypeCache }