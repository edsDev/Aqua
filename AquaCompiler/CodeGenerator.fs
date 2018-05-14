module Aqua.CodeGenerator

open Aqua.Language
open Aqua.Ast
open Aqua.Bytecode
open CollectionUtils

type CodeGenContext =
    { x: int }

let rec compileExpr ctx codeBuf expr =
    let emitBytecode = CodeGen.appendBytecode codeBuf
    let emitExpr = compileExpr ctx codeBuf

    match expr with
    | Ast_InstanceExpr(t) ->
        emitBytecode <| LoadArg 0

    | Ast_LiteralExpr(literal) ->
        match literal with
        | BoolConst(x) ->
            emitBytecode <| PushI32(if x then 1 else 0)
        | IntConst(x) ->
            emitBytecode <| PushI32(x)

    | Ast_NameAccessExpr(_, _, var) ->
        match var with
        | VariableArgument id ->
            emitBytecode <| LoadArg id
        | VariableLocal id ->
            emitBytecode <| LoadLocal id

    | Ast_MemberAccessExpr(_, object, name) ->
        emitExpr <| object
        emitBytecode <| LoadField name

    | Ast_TypeCheckExpr(child, testType) ->
        let childType = getAstExprType child

        if childType.IsReferenceType then
            emitExpr <| child
            
            emitBytecode <| CastObj (getTypeName testType)
            emitBytecode <| CastBool
        else
            match childType, testType with
            | SystemTypeIdent(c1), SystemTypeIdent(c2) ->
                emitBytecode <| PushI32(if c1=c2 then 1 else 0)
            | _ ->
                emitBytecode <| PushI32(0)

    | Ast_TypeCastExpr(child, targetType) ->
        emitExpr <| child

        if (getAstExprType child).IsReferenceType then
            emitBytecode <| CastObj (getTypeName targetType)
        else
            let opcode =
                match targetType with
                | SystemTypeIdent(BuiltinType.Bool)  -> CastBool
                | SystemTypeIdent(BuiltinType.Int)   -> CastI32
                | SystemTypeIdent(BuiltinType.Float) -> CastF32
                | _ -> failwith "not implemented"

            emitBytecode <| opcode

    | Ast_InvocationExpr(callee, args) ->
        args |> List.iter (fun x -> emitExpr <| x)
        match callee with
        | CallableInstanceMethod(instance, methodDef) ->
            // TODO: this is actually incorrect
            //       overloading is not considered yet
            let typeName = instance |> getAstExprType |> getTypeName

            emitExpr <| instance
            emitBytecode <| Call (typeName + "." + methodDef.Name)

        | CallableStaticMethod(klassDef, methodDef) ->
            emitBytecode <| Call (klassDef.Name + "." + methodDef.Name)

        | CallableExpression(calledExpr, sigature) ->
            failwith "expression call not supported yet"

    | Ast_BinaryExpr(_, op, lhs, rhs) ->
        match op with
        | Op_Assign ->
            match lhs with
            | Ast_NameAccessExpr(_, _, var) ->
                emitExpr <| rhs

                match var with
                | VariableArgument id ->
                    emitBytecode <| StoreArg id
                | VariableLocal id ->
                    emitBytecode <| StoreLocal id

            | Ast_MemberAccessExpr(_, object, name) ->
                emitExpr <| rhs
                emitBytecode <| Dup
                emitExpr <| object
                emitBytecode <| StoreField name

            | _ ->
                failwith "impossible case"

        | op ->
            let translationMap = 
                [ Op_Plus, Add 
                  Op_Minus, Sub
                  Op_Asterisk, Mul
                  Op_Slash, Div
                  Op_Modulus, Rem
                  
                  Op_Equal, Eq
                  Op_NotEqual, NEq
                  Op_Greater, Gt
                  Op_GreaterEq, GtEq
                  Op_Less, Ls
                  Op_LessEq, LsEq
                  
                  Op_Conjunction, And
                  Op_Disjunction, Or
                  Op_BitwiseAnd, And
                  Op_BitwiseOr, Or
                  Op_BitwiseXor, Xor ]
                |> DictView.ofSeq

            emitExpr lhs
            emitExpr rhs
            emitBytecode <| DictView.find op translationMap

let rec compileStmt ctx codeBuf stmt =
    let emitBytecode = CodeGen.appendBytecode codeBuf
    let emitStmt = compileStmt ctx codeBuf
    let emitExpr = compileExpr ctx codeBuf

    match stmt with
    | Ast_ExpressionStmt(expr) ->
        emitExpr expr
        emitBytecode <| Pop

    | Ast_VarDeclStmt(id, t, init) ->
        emitExpr init
        emitBytecode <| StoreLocal id

    | Ast_ChoiceStmt(pred, pos, negOpt) ->
        emitExpr pred
        let patchNegJump = CodeGen.appendDummy codeBuf
        emitStmt pos

        match negOpt with
        | Some neg ->
            let patchPosJump = CodeGen.appendDummy codeBuf
            patchNegJump <| JumpOnFalse (CodeGen.extractNextIndex codeBuf)
            emitStmt neg
            patchPosJump <| Jump (CodeGen.extractNextIndex codeBuf)

        | None ->
            patchNegJump <| JumpOnFalse (CodeGen.extractNextIndex codeBuf)
            
    | Ast_WhileStmt(pred, body) ->
        let startIndex = CodeGen.extractNextIndex codeBuf

        emitExpr pred
        let patchNegJump = CodeGen.appendDummy codeBuf
        emitStmt body
        emitBytecode <| Jump startIndex

        patchNegJump <| JumpOnFalse (CodeGen.extractNextIndex codeBuf)

    | Ast_ControlFlowStmt(mode) ->
        ()

    | Ast_ReturnStmt(exprOpt) ->
        match exprOpt with
        | Some expr -> emitExpr expr
        | None -> ()

        emitBytecode <| Ret

    | Ast_CompoundStmt(children) ->
        List.iter emitStmt children

let compileModule = ()