module Aqua.TypeChecker

open Aqua.Language
open Aqua.Syntax
open Aqua.Ast
open Aqua.ErrorMessage
open Aqua.Compiler

open Aqua.Compiler.TranslationContext

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
        |> function AstVariableDecl(_, _, mut) -> mut = MutabilitySpec.Mutable

    | Ast_MemberAccessExpr(_, field) ->
        field.Definition.Mutability = MutabilitySpec.Mutable

    | _ ->
        false

let resolveMethodCallWithType env rg typeName methodName astArgList =
    let argTypeList =
        astArgList |> List.map AstExpr.getType

    match lookupMethod env typeName methodName with
    | Some (klass, candidates) ->
        let matchList =
            candidates
            |> List.filter (fun def -> isInvocable env (MethodDefinition.getSignature def) argTypeList)

        match matchList with
        | [x] ->
            Ok <| makeMethodReference klass x
        | [] ->
            Error <| invalidMethodLookup rg methodName argTypeList
        | _ ->
            Error <| invalidMethodResolve rg methodName argTypeList

    | None ->
        Error <| invalidMethodLookup rg methodName argTypeList

let resolveExplicitInstanceInvocation env rg astInstanceExpr methodName astArgList =
    let srcTypeName =
        astInstanceExpr |> AstExpr.getTypeName

    resolveMethodCallWithType env rg srcTypeName methodName astArgList
    |> Result.bind (fun method ->
                        if ModifierGroup.isPublic method.Definition.Modifiers then
                            if ModifierGroup.isInstance method.Definition.Modifiers then
                                Ok <| AstExpr.createInvokeInstance astInstanceExpr method astArgList
                            else
                                Ok <| AstExpr.createInvokeStatic method astArgList
                        else
                            let argTypeList =
                                    astArgList |> List.map AstExpr.getType

                            Error <| invalidPrivateMethodCall rg methodName argTypeList)

let resolveImplicitInvocation env rg methodName argList =
    let thisTypeName = 
        env |> getCurrentKlassType |> TypeIdent.getTypeName
    
    resolveMethodCallWithType env rg thisTypeName methodName argList
    |> Result.bind (fun method ->
                        if ModifierGroup.isInstance method.Definition.Modifiers then
                            if isInstanceContext env then
                                let thisExpr =
                                    env |> getCurrentKlassType |> AstExpr.createInstance

                                Ok <| AstExpr.createInvokeInstance thisExpr method argList 
                            else
                                let argTypeList =
                                    argList |> List.map AstExpr.getType

                                Error <| invalidInstanceMethodCall rg methodName argTypeList
                        else
                            Ok <| AstExpr.createInvokeStatic method argList)

let resolveExpressionCall env rg calleeExpr argList =
    let argTypeList =
        argList |> List.map AstExpr.getType

    match calleeExpr |> AstExpr.getType with
    | FunctionTypeIdent(signature) when isInvocable env signature argTypeList ->
        Ok <| Ast_InvocationExpr(CallableExpression(calleeExpr, signature), argList)
    | _ ->
        Error <| invalidExpressionCall rg argTypeList

let resolveConstructor env rg typeName argList =
    resolveMethodCallWithType env rg typeName "$ctor" argList
    |> Result.map (fun ctor -> AstExpr.createNewObject ctor argList)

// helpers
//

let translateSyntaxList translateFunc synList ctx =
    synList |> List.mapFold (fun cc syn -> translateFunc syn cc) ctx

let rec translateType synType = translateTypeM {
    let! ctx = getContext
    
    match synType with
    | Syn_SystemType(_, category) ->
        return SystemTypeIdent category
    | Syn_UserType(rg, name) ->
        match lookupKlass ctx name with
        | Some item -> return item.Type
        | None      -> yield invalidUserType rg name
    | Syn_FunctionType(_, paramTypeList, retType) ->
        let! paramTypeList' = translateSyntaxList translateType paramTypeList
        let! retType' = translateType retType

        return TypeIdent.makeFunctionType paramTypeList' retType'
}

let rec translateExpr synExpr ctx =
    ctx |>
    match synExpr with
    | Syn_InstanceExpr(rg)                      -> translateInstanceExpr ctx rg
    | Syn_LiteralExpr(rg, value)                -> translateLiteralExpr ctx rg value
    | Syn_NameAccessExpr(rg, name)              -> translateNameAccessExpr ctx rg name
    | Syn_MemberAccessExpr(rg, child, name)     -> translateMemberAccessExpr ctx rg child name
    | Syn_TypeCheckExpr(rg, child, testType)    -> translateTypeCheckExpr ctx rg child testType
    | Syn_TypeCastExpr(rg, child, targetType)   -> translateTypeCastExpr ctx rg child targetType
    | Syn_InvocationExpr(rg, callee, args)      -> translateInvocationExpr ctx rg callee args
    | Syn_NewObjectExpr(rg, type', args)        -> translateNewObjectExpr ctx rg type' args
    | Syn_BinaryExpr(rg, op, lhs, rhs)          -> translateBinaryExpr ctx rg op lhs rhs

and translateInstanceExpr env rg = translateExprM {
    if isInstanceContext env then
        return AstExpr.createInstance (getCurrentKlassType env)
    else
        yield invalidInstanceReference rg 
    }

and translateLiteralExpr env rg value = translateExprM {
    return AstExpr.createLiteral value
    }

and translateNameAccessExpr env rg name = translateExprM {
    match lookupVariable env name with
    | Some (ArgumentLookupItem(id, _, t)) ->
        return AstExpr.createArgumentAccess t id

    | Some (VariableLookupItem(id, _, t, _)) ->
        return AstExpr.createLocalAccess t id

    | None ->
        let typeName = 
            env |> getCurrentKlassType |> TypeIdent.getTypeName

        match lookupField env typeName name with
        | Some (klass, field) ->
            let thisExpr =
                env |> getCurrentKlassType |> AstExpr.createInstance
            let fieldRef = 
                makeFieldReference klass field

            return AstExpr.createFieldAccess thisExpr fieldRef

        | None ->
            yield invalidVariableReference rg name
    }

and translateMemberAccessExpr env rg synSrcExpr fieldName = translateExprM {
    let! astSrcExpr = translateExpr synSrcExpr
    let typeName = AstExpr.getTypeName astSrcExpr
    match lookupField env typeName fieldName with
    | Some (klass, field) ->
        return AstExpr.createFieldAccess astSrcExpr (makeFieldReference klass field)
    | None ->
        yield invalidFieldReference rg typeName fieldName
    }

and translateTypeCheckExpr env rg synExprToCheck synTestType = translateExprM {
    let! exprToCheck = translateExpr synExprToCheck
    let! testType = translateType synTestType

    return Ast_TypeCheckExpr(exprToCheck, testType)
    }

and translateTypeCastExpr env rg synExprToCast synTargetType = translateExprM {
    let! exprToCast = translateExpr synExprToCast
    let! targetType = translateType synTargetType

    return Ast_TypeCastExpr(exprToCast, targetType)
    }

and translateInvocationExpr env rg synCalledExpr args = translateExprM {

    // try translate arguments provided
    let! astArgs = translateSyntaxList translateExpr args

    match synCalledExpr with
    | Syn_NameAccessExpr(nameRange, methodName) ->
        match resolveImplicitInvocation env nameRange methodName astArgs with
        | Ok ast -> return ast
        | Error e -> yield e
    | Syn_MemberAccessExpr(nameRange, synSrcExpr, methodName) when true ->
        let! astSrcExpr = translateExpr synSrcExpr
        match resolveExplicitInstanceInvocation env nameRange astSrcExpr methodName astArgs with
        | Ok ast -> return ast
        | Error e -> yield e
    | _ ->
        let! astCalledExpr = translateExpr synCalledExpr
        match resolveExpressionCall env synCalledExpr.Range astCalledExpr astArgs with
        | Ok ast -> return ast
        | Error e -> yield e
    }

and translateNewObjectExpr env rg synType synArgList = translateExprM {
    // try translate arguments provided
    let! astArgList = translateSyntaxList translateExpr synArgList
    let! astType = translateType synType
    match resolveConstructor env rg (astType |> TypeIdent.getTypeName) astArgList with
    | Ok ast -> return ast
    | Error e -> yield e
    }

and translateBinaryExpr env rg op synLhs synRhs = translateExprM {
    let processBuiltinBinaryExpr e1 e2 =
        let lhsType = AstExpr.getType e1
        let rhsType = AstExpr.getType e2

        assert lhsType.IsBuiltinType
        assert rhsType.IsBuiltinType

        match op with
        | Op_Assign ->
            if not <| isAssignable env e1 then
                Error <| invalidAssignment (SyntaxExpr.getRange synLhs)
            else if not <| existImplicitConversion env rhsType lhsType then
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
            | SystemTypeIdent(x), SystemTypeIdent(y) when x=y && x<>BuiltinTypeToken.Ty_Unit ->
                Ok <| Ast_BinaryExpr(TypeIdent.kBoolType, op, e1, e2)
            | _ ->
                Error <| invalidBinaryOperation rg op lhsType rhsType

        | Op_Conjunction | Op_Disjunction ->
            match lhsType, rhsType with
            | SystemTypeIdent(BuiltinTypeToken.Ty_Bool), SystemTypeIdent(BuiltinTypeToken.Ty_Bool) ->
                Ok <| Ast_BinaryExpr(TypeIdent.kBoolType, op, e1, e2)
            | _ ->
                Error <| invalidBinaryOperation rg op lhsType rhsType

    let processUserBinaryExpr e1 e2 =
        failwith "operator overloading not supported yet"

    let! astLhs = translateExpr synLhs
    let! astRhs = translateExpr synRhs

    match astLhs.Type, astRhs.Type with
    | TypeIdent.OfBuiltinType(_), TypeIdent.OfBuiltinType(_) ->
        match processBuiltinBinaryExpr astLhs astRhs with
        | Ok ast -> return ast
        | Error e -> yield e
    | _ ->
        return processUserBinaryExpr astLhs astRhs
    }

let translateExprAssertType env rg constraintT synExpr = translateExprM {
    let! astExpr = translateExpr synExpr
    let exprType = astExpr |> AstExpr.getType

    if existImplicitConversion env exprType constraintT then
        return astExpr
    else
        yield invalidImpilicitConversion synExpr.Range exprType constraintT
    }

let rec translateStmt synStmt ctx =
    ctx |>
    match synStmt with
    | Syn_ExpressionStmt(rg, expr)                      -> translateExpressionStmt ctx rg expr
    | Syn_VarDeclStmt(rg, mut, name, typeAnnot, init)   -> translateVarDeclStmt ctx rg mut name typeAnnot init
    | Syn_ChoiceStmt(rg, pred, pBranch, nBranch)        -> translateChoiceStmt ctx rg pred pBranch nBranch
    | Syn_WhileStmt(rg, pred, body)                     -> translateWhileStmt ctx rg pred body
    | Syn_ControlFlowStmt(rg, ctrl)                     -> translateControlFlowStmt ctx rg ctrl
    | Syn_ReturnStmt(rg, maybeExpr)                     -> translateReturnStmt ctx rg maybeExpr
    | Syn_CompoundStmt(rg, children)                    -> translateCompoundStmt ctx rg children

and translateExpressionStmt env rg synExpr = translateStmtM {
    let! astExpr = translateExpr synExpr

    match astExpr with
    | Ast_BinaryExpr(_, Op_Assign, _, _)
    | Ast_InvocationExpr(_, _) ->
        return Ast_ExpressionStmt astExpr
    | _ ->
        yield invalidExpressionStmt rg
    }

and translateVarDeclStmt env rg mut name synTypeAnnotOpt synInit = translateStmtM {
    match lookupVariable env name with
    | Some _ ->
        yield invalidVariableDecl rg name
    | None ->
        let! astInit = translateExpr synInit
        let initType = AstExpr.getType astInit
        let id = getNextVarId env

        match synTypeAnnotOpt with
        | Some synTypeAnnot ->
            let! declType = translateType synTypeAnnot
            do! declareVariable name mut declType

            if existImplicitConversion env initType declType then
                return Ast_VarDeclStmt(id, declType, astInit)
            else
                yield invalidImpilicitConversion rg initType declType
        | None ->
            do! declareVariable name mut initType
            return Ast_VarDeclStmt(id, initType, astInit)
    }

and translateChoiceStmt env rg synPred synPBody synNBodyOpt = translateStmtM {
    let! astPred = translateExprAssertType env () (TypeIdent.kBoolType) synPred
    let! astPBody = translateStmt synPBody
    match synNBodyOpt with
    | Some synNBody ->
        let! astNBody = translateStmt synNBody
        return Ast_ChoiceStmt(astPred, astPBody, Some astNBody)
    | None ->
        return Ast_ChoiceStmt(astPred, astPBody, None)
    }

and translateWhileStmt env rg synPred synBody = translateStmtM {
    let! astPred = translateExprAssertType env () (TypeIdent.kBoolType) synPred
    do! enterLoopBody
    let! astBody = translateStmt synBody
    do! exitLoopBody

    return Ast_WhileStmt(astPred, astBody)
    }

and translateControlFlowStmt env rg ctrl = translateStmtM {
    if insideLoopBody env then
        return Ast_ControlFlowStmt ctrl
    else
        yield invalidControlFlow rg ctrl
   }

and translateReturnStmt env rg synRetValueOpt = translateStmtM {
    let returnType = getCurrentReturnType env

    match synRetValueOpt with
    | Some synRetValue ->
        let! astRetValue = translateExprAssertType env () returnType synRetValue
        return Ast_ReturnStmt(Some astRetValue)
    | None ->
        if returnType.IsUnitType then
            return Ast_ReturnStmt None
        else
            yield invalidImpilicitConversion rg TypeIdent.kUnitType returnType
    }

and translateCompoundStmt env rg synBodyList = translateStmtM {
    let! astBodyList = translateSyntaxList translateStmt synBodyList
    do! restoreScope env

    return Ast_CompoundStmt astBodyList
    }

let translateMethod env synBody =
    let astBody, ctx = translateStmt synBody (createContext env)

    if ctx.ErrorMessages.IsEmpty then
        Ok <| AstMethod(env.CurrentMethod, ctx.VariableList, astBody)
    else
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
