module Aqua.TypeChecker

open Aqua.Language
open Aqua.Syntax
open Aqua.ErrorMessage
open Aqua.Compiler

open Aqua.Compiler.TranslationEnvironment
open Aqua.Compiler.TranslationContext

let (|UnaryWildcard|_|) t =
    match t with
    | WildcardType -> Some ()
    | _ -> None

let (|BinaryWildcard|_|) ts =
    match ts with
    | WildcardType, _ 
    | _, WildcardType -> Some()
    | _ -> None

let existImplicitConversion env srcType destType =
    match srcType, destType with
    | WildcardType, _                   -> true
    | _, WildcardType                   -> true
    | SystemType(t1), SystemType(t2)    -> t1=t2
    | _                                 -> false

let isInvocable env signature argTypeList =
    let (FunctionSignature(paramTypeList, _)) = signature

    List.length paramTypeList = List.length argTypeList &&
    List.forall2 (existImplicitConversion env) argTypeList paramTypeList

let rec checkExpr env ctx expr =

    // shortcuts for ease of recursive invocation
    //
    let checkExpr = checkExpr env

    // auxiliary functions
    //
    let ensureTypeExistence type' ctx =
        ctx

    let appendExprError expr msg ctx =
        ctx
        |> cacheExprType expr WildcardType
        |> appendError msg

    //
    //
    match expr with
    | LiteralExpr(_, value) ->
        match value with
        | BoolConst(_) -> ctx |> cacheExprType expr kBoolType
        | IntConst(_) -> ctx |> cacheExprType expr kIntType

    | NamedExpr(rg, name) ->
        match ctx.VariableLookup |> Map.tryFind name with
        | Some(VariableLookupItem(_, _, t)) -> 
            ctx 
            |> cacheExprType expr t
        | None ->
            ctx 
            |> appendExprError expr (invalidVariableReference rg name)

    // 
    | InvocationExpr(_, f, args) ->
        let ctxOnArgs = args |> List.scan checkExpr ctx |> List.tail
        
        let currentCtx = ctxOnArgs |> List.last
        let argTypeList = ctxOnArgs |> List.map (fun tmpCtx -> tmpCtx.LastExprType)

        // TODO: remove duplicate code
        match f with
        // call function
        | NamedExpr(nameRange, funcName) when ctx.VariableLookup |> Map.containsKey funcName |> not ->
            // TODO: refine overload resolving
            let functions = 
                lookupFunction env "" funcName
                |> List.filter (fun def -> isInvocable env def.Signature argTypeList)

            match functions with
            | [x] ->
                currentCtx
                |> cacheExprType expr x.Signature.ReturnType
            | [] ->
                currentCtx
                |> appendExprError expr (invalidFunctionCall nameRange funcName argTypeList)
            | _ ->
                currentCtx
                |> appendExprError expr (invalidFunctionResolve nameRange funcName argTypeList)

        // call expression
        | _ ->
            let currentCtx2, fType = withLastExprType <| checkExpr currentCtx f
            match fType with
            | FunctionType(signature) when isInvocable env signature argTypeList ->
                currentCtx2
                |> cacheExprType expr signature.ReturnType
            | _ ->
                currentCtx2
                |> appendExprError expr (invalidExpressionCall f.Range argTypeList)

    | TypeCheckExpr(_, child, type') ->
        checkExpr ctx child
        |> ensureTypeExistence type'
        |> cacheExprType expr kBoolType

    | TypeCastExpr(_, child, type') ->
        checkExpr ctx child
        |> ensureTypeExistence type'
        |> cacheExprType expr type'

    | BinaryExpr(rg, op, lhs, rhs) ->
        let newContext, lhsType, rhsType =
            let c1, lt = withLastExprType <| checkExpr ctx lhs
            let c2, rt = withLastExprType <| checkExpr c1 rhs

            c2, lt, rt

        match op with
        | Op_Assign ->
            if existImplicitConversion env rhsType lhsType then
                newContext
                |> cacheExprType expr lhsType
            else
                newContext
                |> appendExprError expr (invalidBinaryOperation rg op lhsType rhsType)

        | Op_Plus | Op_Minus
        | Op_Asterisk | Op_Slash
        | Op_Modulus 
        | Op_BitwiseAnd | Op_BitwiseOr
        | Op_BitwiseXor ->
            match lhsType, rhsType with
            | BinaryWildcard ->
                newContext 
                |> cacheExprType expr WildcardType
            | SystemType(Int), SystemType(Int) ->
                newContext 
                |> cacheExprType expr kIntType
            | SystemType(_), SystemType(_) ->
                newContext 
                |> appendExprError expr (invalidBinaryOperation rg op lhsType rhsType)
            | _ -> 
                failwith "user type not supported yet"

        | Op_Equal | Op_NotEqual
        | Op_Greater | Op_GreaterEq
        | Op_Less | Op_LessEq ->
            match lhsType, rhsType with
            | BinaryWildcard ->
                newContext 
                |> cacheExprType expr kBoolType
            | SystemType(x), SystemType(y) when x=y && x<>Unit ->
                newContext 
                |> cacheExprType expr kBoolType
            | SystemType(_), SystemType(_) ->
                newContext 
                |> cacheExprType expr kBoolType
                |> appendError (invalidBinaryOperation rg op lhsType rhsType)
            | _ -> 
                failwith "user type not supported yet"

        | Op_Conjunction | Op_Disjunction ->
            match lhsType, rhsType with
            | BinaryWildcard
            | SystemType(Bool), SystemType(Bool) ->
                newContext 
                |> cacheExprType expr kBoolType
            | SystemType(_), SystemType(_) ->
                newContext 
                |> cacheExprType expr kBoolType
                |> appendError (invalidBinaryOperation rg op lhsType rhsType)
            | _ -> 
                failwith "user type not supported yet"

let rec checkStmt env ctx stmt =
    
    // shortcuts for ease of recursive invocation
    //
    let checkExpr = checkExpr env
    let checkStmt = checkStmt env

    // auxiliary functions
    //
    let ensureTypeConversion rg srcType destType ctx =
        if existImplicitConversion env srcType destType then
            ctx
        else
            ctx |> appendError (invalidImpilicitConversion rg srcType destType)

    let ensureExprType expr constraintType ctx =
        let newContext, exprType = withLastExprType <| checkExpr ctx expr
        
        ensureTypeConversion expr.Range exprType constraintType newContext

    //
    //
    match stmt with
    | ExpressionStmt(rg, expr) ->
        let newContext = checkExpr ctx expr
        match expr with
        | BinaryExpr(_, Op_Assign, _, _)
        | InvocationExpr(_, _, _) ->
            newContext
        | _ ->
            newContext
            |> appendError (invalidExpressionStmt rg)

    | VarDeclStmt(rg, mut, name, type', init) ->
        let newContext, exprType = withLastExprType <| checkExpr ctx init

        match type' with
        | Some t ->
            ensureTypeConversion rg exprType t newContext
            |> declareVariable name mut t
        | None -> 
            newContext 
            |> declareVariable name mut exprType

    | ChoiceStmt(_, pred, posiBranch, negaBranch) ->
        let newContext = ensureExprType pred kBoolType ctx
        
        [ Some posiBranch; negaBranch; ]
        |> List.choose id
        |> List.fold checkStmt newContext

    | WhileStmt(_, pred, body) ->
        ensureExprType pred kBoolType ctx
        |> enterLoopBody
        |> (fun ctx -> checkStmt ctx body)
        |> exitLoopBody

    | ControlFlowStmt(rg, ctrl) ->
        if ctx.LoopDepth > 0 then
            ctx
        else
            ctx |> appendError (invalidControlFlow rg ctrl)

    | ReturnStmt(rg, maybeExpr) ->
        let newContext, exprType =
            match maybeExpr with
            | Some(expr) -> withLastExprType <| checkExpr ctx expr
            | None       -> ctx, kUnitType

        ensureTypeConversion rg exprType (ctx.ReturnType) newContext

    | CompoundStmt(_, children) ->
        List.fold checkStmt ctx children |> restoreScope ctx

let checkFunction env func =
    let ctx = checkStmt env (TranslationContext.createContext func) (func.Body)
    ctx
    //{ ExprTypeLookup = ctx.ExprTypeCache }