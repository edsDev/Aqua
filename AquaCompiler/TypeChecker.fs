﻿module Aqua.TypeChecker

open Aqua.Language
open Aqua.Syntax
open Aqua.Ast
open Aqua.ErrorMessage
open Aqua.Compiler

open Aqua.Compiler.TranslationContext
open Aqua.Ast

let existImplicitConversion ctx srcType destType =
    srcType = destType

let isInvocable ctx signature argTypeList =
    let (FunctionSignature(paramTypeList, _)) = signature

    List.length paramTypeList = List.length argTypeList &&
    List.forall2 (existImplicitConversion ctx) argTypeList paramTypeList

let isAssignable ctx expr =
    match expr with
    | Ast_NameAccessExpr(_, VariableArgument(_)) ->
        false

    | Ast_NameAccessExpr(_, VariableLocal(id)) ->
        lookupVariableById ctx id 
        |> function 
           | AstVariableDecl(_, _, mut) -> mut = MutabilitySpec.Mutable

    | Ast_MemberAccessExpr(_, field) ->
        field.Definition.Mutability = MutabilitySpec.Mutable

    | _ ->
        false

let resolveMethodCallWithType ctx rg typeName methodName argList =
    let argTypeList =
        argList |> List.map AstExpr.getType

    match lookupMethod ctx typeName methodName with
    | Some (klass, candidates) ->
        let matchList =
            candidates
            |> List.filter (fun def -> isInvocable ctx (MethodDefinition.getSignature def) argTypeList)

        match matchList with
        | [x] ->
            Ok <| makeMethodReference klass x
        | [] ->
            Error <| invalidMethodLookup rg methodName argTypeList
        | _ ->
            Error <| invalidMethodResolve rg methodName argTypeList

    | None ->
        Error <| invalidMethodLookup rg methodName argTypeList

let resolveExplicitInstanceInvocation ctx rg srcExpr methodName argList =
    let srcTypeName =
        srcExpr |> AstExpr.getTypeName

    resolveMethodCallWithType ctx rg srcTypeName methodName argList
    |> Result.bind (fun method ->
                        if ModifierGroup.isPublic method.Definition.Modifiers then
                            if ModifierGroup.isInstance method.Definition.Modifiers then
                                Ok <| AstExpr.createInvokeInstance srcExpr method argList
                            else
                                Ok <| AstExpr.createInvokeStatic method argList
                        else
                            let argTypeList =
                                    argList |> List.map AstExpr.getType

                            Error <| invalidPrivateMethodCall rg methodName argTypeList)

let resolveImplicitInvocation ctx rg methodName argList =
    let thisTypeName = 
        ctx |> getCurrentKlassType |> TypeIdent.getTypeName
    
    resolveMethodCallWithType ctx rg thisTypeName methodName argList
    |> Result.bind (fun method ->
                        if ModifierGroup.isInstance method.Definition.Modifiers then
                            if isInstanceContext ctx then
                                let thisExpr =
                                    ctx |> getCurrentKlassType |> AstExpr.createInstance

                                Ok <| AstExpr.createInvokeInstance thisExpr method argList 
                            else
                                let argTypeList =
                                    argList |> List.map AstExpr.getType

                                Error <| invalidInstanceMethodCall rg methodName argTypeList
                        else
                            Ok <| AstExpr.createInvokeStatic method argList)

let resolveExpressionCall ctx rg calleeExpr argList =
    let argTypeList =
        argList |> List.map AstExpr.getType

    match calleeExpr |> AstExpr.getType with
    | FunctionTypeIdent(signature) when isInvocable ctx signature argTypeList ->
        Ok <| Ast_InvocationExpr(CallableExpression(calleeExpr, signature), argList)
    | _ ->
        Error <| invalidExpressionCall rg argTypeList

let resolveConstructor ctx rg typeName argList =
    resolveMethodCallWithType ctx rg typeName "$ctor" argList
    |> Result.map (fun ctor -> AstExpr.createNewObject ctor argList)
    

(*
type SyntaxTypeTranslator =
    TranslationContext -> SyntaxType -> TypeIdent option*TranslationContext
type ExpressionTranslator =
    TranslationContext -> SyntaxExpr -> AstExpr option*TranslationContext
type StatementTranslator =
    TranslationContext -> SyntaxStmt -> AstStmt option*TranslationContext
*)

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
        match lookupKlass ctx name with
        | Some item -> ctx |> makeEvalResult item.Type
        | None      -> ctx |> makeEvalError (invalidUserType rg name)

    | Syn_FunctionType(_, paramTypeList, retType) ->
        retType::paramTypeList
        |> List.mapFold translateType ctx
        |> processEvalResultList
               (fun ts ->
                    let paramsStub = ts |> List.tail
                    let returnStub = ts |> List.head

                    Ok <| TypeIdent.makeFunctionType paramsStub returnStub)

let rec translateExpr ctx expr =

    // case-wise translate functions
    //
    let translateInstanceExpr rg =
        if isInstanceContext ctx then
            let typeAnnot = 
                getCurrentKlassType ctx

            ctx |> makeEvalResult (AstExpr.createInstance typeAnnot)
        else
            ctx |> makeEvalError (invalidInstanceReference rg)

    let translateLiteralExpr rg value =
        ctx |> makeEvalResult (AstExpr.createLiteral value)

    let translateNameAccessExpr rg name =
        match lookupVariable ctx name with
        | Some (ArgumentLookupItem(id, _, t)) ->
            ctx |> makeEvalResult (AstExpr.createArgumentAccess t id)

        | Some (VariableLookupItem(id, _, t, _)) ->
            ctx |> makeEvalResult (AstExpr.createLocalAccess t id)

        | None ->
            let typeName = 
                ctx |> getCurrentKlassType |> TypeIdent.getTypeName

            match lookupField ctx typeName name with
            | Some (klass, field) ->
                let thisExpr =
                    ctx |> getCurrentKlassType |> AstExpr.createInstance
                let fieldRef = 
                    makeFieldReference klass field

                ctx |> makeEvalResult (AstExpr.createFieldAccess thisExpr fieldRef)

            | None ->
                ctx |> makeEvalError (invalidVariableReference rg name)

    let translateMemberAccessExpr rg srcExpr name =
        translateExpr ctx srcExpr
        |> processEvalResult
               (fun e ->
                    let typeName = e |> AstExpr.getTypeName
                    match lookupField ctx typeName name with
                    | Some (klass, field) ->
                        Ok <| AstExpr.createFieldAccess e (makeFieldReference klass field)

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
            let astArgList = maybeAstArgs |> List.map Option.get

            match calleeExpr with
            | Syn_NameAccessExpr(nameRange, methodName) ->
                newCtx 
                |> processReturn (resolveImplicitInvocation ctx nameRange methodName astArgList)
            | Syn_MemberAccessExpr(nameRange, srcExpr, methodName) when true ->
                translateExpr newCtx srcExpr
                |> processEvalResult
                       (fun e ->
                            resolveExplicitInstanceInvocation ctx nameRange e methodName astArgList)
            | _ ->
                translateExpr newCtx calleeExpr
                |> processEvalResult
                       (fun e ->
                            resolveExpressionCall ctx calleeExpr.Range e astArgList)

        else
            newCtx |> escapeEvalError

    let translateNewObjectExpr rg type' args =

        // try translate arguments provided
        let maybeAstArgs, newCtx = List.mapFold translateExpr ctx args

        if maybeAstArgs |> List.forall Option.isSome then
            let astArgList = maybeAstArgs |> List.map Option.get

            translateType newCtx type'
            |> processEvalResult
                   (fun t -> resolveConstructor ctx rg (t |> TypeIdent.getTypeName) astArgList)

        else
            newCtx |> escapeEvalError

    let translateBinaryExpr rg op lhs rhs =
        let processBuiltinBinaryExpr e1 e2 =
            let lhsType = AstExpr.getType e1
            let rhsType = AstExpr.getType e2

            assert lhsType.IsBuiltinType
            assert rhsType.IsBuiltinType

            match op with
            | Op_Assign ->
                if not <| isAssignable ctx e1 then
                    Error <| invalidAssignment (SyntaxExpr.getRange lhs)
                else if not <| existImplicitConversion ctx rhsType lhsType then
                    Error <| invalidBinaryOperation rg op lhsType rhsType
                else
                    Ok <| Ast_BinaryExpr(lhsType, op, e1, e2)

            // simple arithmetic operations
            | Op_Plus | Op_Minus
            | Op_Asterisk | Op_Slash ->
                match lhsType, rhsType with
                | TypeIdent.OfIntType, TypeIdent.OfIntType ->
                    Ok <| Ast_BinaryExpr(TypeIdent.kIntType, op, e1, e2)
                | TypeIdent.OfFloatType, TypeIdent.OfFloatType ->
                    Ok <| Ast_BinaryExpr(TypeIdent.kFloatType, op, e1, e2)
                | _ ->
                    Error <| invalidBinaryOperation rg op lhsType rhsType

            // int-only operations
            | Op_Modulus
            | Op_BitwiseAnd | Op_BitwiseOr
            | Op_BitwiseXor ->
                match lhsType, rhsType with
                | TypeIdent.OfIntType, TypeIdent.OfIntType ->
                    Ok <| Ast_BinaryExpr(TypeIdent.kIntType, op, e1, e2)
                | _ ->
                    Error <| invalidBinaryOperation rg op lhsType rhsType

            | Op_Equal | Op_NotEqual
            | Op_Greater | Op_GreaterEq
            | Op_Less | Op_LessEq ->
                match lhsType, rhsType with
                | SystemTypeIdent(x), SystemTypeIdent(y) when x=y && x<>BuiltinType.Unit ->
                    Ok <| Ast_BinaryExpr(TypeIdent.kBoolType, op, e1, e2)
                | _ ->
                    Error <| invalidBinaryOperation rg op lhsType rhsType

            | Op_Conjunction | Op_Disjunction ->
                match lhsType, rhsType with
                | SystemTypeIdent(BuiltinType.Bool), SystemTypeIdent(BuiltinType.Bool) ->
                    Ok <| Ast_BinaryExpr(TypeIdent.kBoolType, op, e1, e2)
                | _ ->
                    Error <| invalidBinaryOperation rg op lhsType rhsType

        let processUserBinaryExpr e1 e2 =
            failwith "operator overloading not supported yet"

        translatePair (translateExpr, translateExpr) ctx lhs rhs
        |> processEvalResult2
               (fun e1 e2 ->
                    match e1.Type, e2.Type with
                    | TypeIdent.OfBuiltinType(_), TypeIdent.OfBuiltinType(_) ->
                        processBuiltinBinaryExpr e1 e2
                    | _ ->
                        processUserBinaryExpr e1 e2)





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
    | Syn_NewObjectExpr(rg, type', args)        -> translateNewObjectExpr rg type' args
    | Syn_BinaryExpr(rg, op, lhs, rhs)          -> translateBinaryExpr rg op lhs rhs

let rec translateStmt ctx stmt =

    // auxiliary functions
    //

    let ensureExprType expr constraintType ctx =
        translateExpr ctx expr
        |> processEvalResult
               (fun e ->
                    let exprType = e |> AstExpr.getType
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
                        let initType = AstExpr.getType value
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
        ensureExprType pred TypeIdent.kBoolType ctx
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
        ensureExprType pred TypeIdent.kBoolType ctx
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
            ensureExprType retValue (getCurrentReturnType ctx) ctx
            |> processEvalResult (fun e -> Ok <| Ast_ReturnStmt (Some e))
        | None ->
            let destType = getCurrentReturnType ctx
            if destType.IsUnitType then
                ctx |> makeEvalResult (Ast_ReturnStmt None)
            else
                ctx |> makeEvalError (invalidImpilicitConversion rg TypeIdent.kUnitType destType)

    let translateCompoundStmt rg children =
        List.mapFold translateStmt ctx children
        |> processEvalResultList (Ok << Ast_CompoundStmt)
        |> updateContext (restoreScope ctx)

    //
    //
    match stmt with
    | Syn_ExpressionStmt(rg, expr)                      -> translateExpressionStmt rg expr
    | Syn_VarDeclStmt(rg, mut, name, typeAnnot, init)   -> translateVarDeclStmt rg mut name typeAnnot init
    | Syn_ChoiceStmt(rg, pred, pBranch, nBranch)        -> translateChoiceStmt rg pred pBranch nBranch
    | Syn_WhileStmt(rg, pred, body)                     -> translateWhileStmt rg pred body
    | Syn_ControlFlowStmt(rg, ctrl)                     -> translateControlFlowStmt rg ctrl
    | Syn_ReturnStmt(rg, maybeExpr)                     -> translateReturnStmt rg maybeExpr
    | Syn_CompoundStmt(rg, children)                    -> translateCompoundStmt rg children

let translateMethod env body =
    let result, ctx = translateStmt (createContext env) body

    match result with
    | Some s ->
        Ok <| AstMethod(env.CurrentMethod, ctx.VariableList, s)
    | None ->
        Error <| ctx.ErrorMessages

let translateModule session =
    let errorBuffer = ResizeArray()

    let klassList = Seq.toList <| seq {
        for PendingKlass(klassDef, methodList) in session.PendingKlassList do
            let methods = Seq.toList <| seq {
                for PendingMethod(methodDef, body) in methodList do
                    let env = { CurrentModule = session.CurrentModule.ModuleName
                                CurrentKlass = klassDef
                                CurrentMethod = methodDef
                                TypeLookup = session.TypeLookup }

                    match translateMethod env body with
                    | Ok result -> yield result
                    | Error msgs -> errorBuffer.AddRange(msgs)
            }

            yield AstKlass(klassDef, methods)
        }
    
    if errorBuffer.Count = 0 then
        Ok <| AstModule(session.CurrentModule.ModuleName, klassList)
    else
        Error <| TranslationError errorBuffer
