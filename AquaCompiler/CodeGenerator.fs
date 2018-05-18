module Aqua.CodeGenerator

open Aqua.Language
open Aqua.Ast
open Aqua.Bytecode
open CollectionUtils

type CodeGenContext =
    { LoopStart: int }

let rec compileExpr ctx codeBuf expr =
    let emitBytecode = CodeGen.appendBytecode codeBuf
    let emitExpr = compileExpr ctx codeBuf

    match expr with
    | Ast_InstanceExpr(t) ->
        emitBytecode <| LoadArg 0

    | Ast_LiteralExpr(literal) ->
        match literal with
        | BoolConst(x) ->
            emitBytecode <| PushI32 (if x then 1 else 0)
        | IntConst(x) ->
            emitBytecode <| PushI32 x

    | Ast_NameAccessExpr(_, var) ->
        match var with
        | VariableArgument id ->
            emitBytecode <| LoadArg id
        | VariableLocal id ->
            emitBytecode <| LoadLocal id

    | Ast_MemberAccessExpr(object, field) ->
        emitExpr <| object
        emitBytecode <| LoadField (field.Definition.Name)

    | Ast_TypeCheckExpr(child, testType) ->
        if child.Type.IsReferenceType then
            emitExpr <| child
            
            emitBytecode <| CastObj child.Type
            emitBytecode <| CastBool
        else
            match child.Type, testType with
            | SystemTypeIdent(c1), SystemTypeIdent(c2) ->
                emitBytecode <| PushI32 (if c1=c2 then 1 else 0)
            | _ ->
                emitBytecode <| PushI32 0

    | Ast_TypeCastExpr(child, targetType) ->
        emitExpr <| child

        if child.Type.IsReferenceType then
            emitBytecode <| CastObj targetType
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
        | CallableInstanceMethod(instance, methodRef) ->
            // TODO: this is actually incorrect
            //       overloading is not considered yet
            let typeName = instance |> AstExpr.getType |> getTypeName

            emitExpr <| instance
            emitBytecode <| Call (typeName + "." + methodRef.Definition.Name)

        | CallableStaticMethod(methodRef) ->
            emitBytecode <| Call (methodRef.Klass.Name + "." + methodRef.Definition.Name)

        | CallableExpression(calledExpr, sigature) ->
            failwith "expression call not supported yet"

    | Ast_BinaryExpr(_, op, lhs, rhs) ->
        match op with
        | Op_Assign ->
            match lhs with
            | Ast_NameAccessExpr(_, var) ->
                emitExpr <| rhs

                match var with
                | VariableArgument id ->
                    emitBytecode <| StoreArg id
                | VariableLocal id ->
                    emitBytecode <| StoreLocal id

            | Ast_MemberAccessExpr(object, field) ->
                emitExpr <| rhs
                emitBytecode <| Dup
                emitExpr <| object
                emitBytecode <| StoreField (field.Definition.Name)

            | _ ->
                failwith "impossible case"

        | op ->
            emitExpr <| lhs
            emitExpr <| rhs
            emitBytecode <| DictView.find op kBinaryOpTranslationMap

and kBinaryOpTranslationMap = 
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

// TODO: eliminate `ignore`
let rec compileStmt ctx codeBuf stmt =
    let emitBytecode = CodeGen.appendBytecode codeBuf
    let emitStmt = compileStmt ctx codeBuf
    let emitExpr = compileExpr ctx codeBuf

    match stmt with
    | Ast_ExpressionStmt(expr) ->
        emitExpr expr
        emitBytecode <| Pop

        ignore

    | Ast_VarDeclStmt(id, t, init) ->
        emitExpr init
        emitBytecode <| StoreLocal id

        ignore

    | Ast_ChoiceStmt(pred, pos, negOpt) ->
        emitExpr pred
        let patchNegJump = CodeGen.appendDummy codeBuf
        let patchPosBreak = emitStmt pos

        match negOpt with
        | Some neg ->
            let patchPosJump = CodeGen.appendDummy codeBuf
            patchNegJump <| JumpOnFalse (CodeGen.extractNextIndex codeBuf)
            let patchNegBreak = emitStmt neg
            patchPosJump <| Jump (CodeGen.extractNextIndex codeBuf)

            (fun bytecode -> do patchPosBreak bytecode; do patchNegBreak bytecode)

        | None ->
            patchNegJump <| JumpOnFalse (CodeGen.extractNextIndex codeBuf)
            
            patchPosBreak

    | Ast_WhileStmt(pred, body) ->
        let startIndex = CodeGen.extractNextIndex codeBuf

        emitExpr pred
        let patchNegJump = CodeGen.appendDummy codeBuf
        let patchBreak = compileStmt { LoopStart = startIndex } codeBuf body
        emitBytecode <| Jump startIndex

        patchNegJump <| JumpOnFalse (CodeGen.extractNextIndex codeBuf)
        patchBreak <| Jump (CodeGen.extractNextIndex codeBuf)

        ignore

    | Ast_ControlFlowStmt(mode) ->
        match mode with
        | ControlFlow.Break -> 
            CodeGen.appendDummy codeBuf
        | ControlFlow.Continue ->
            emitBytecode <| Jump ctx.LoopStart
            ignore

    | Ast_ReturnStmt(exprOpt) ->
        match exprOpt with
        | Some expr -> emitExpr expr
        | None -> ()

        emitBytecode <| Ret

        ignore

    | Ast_CompoundStmt(children) ->
        let patchList =
            List.map emitStmt children
            //|> List.filter (fun patch -> obj.ReferenceEquals(patch, ignore) |> not)

        fun bytecode -> patchList |> List.iter (fun patch -> patch bytecode)

let compileModule = ()