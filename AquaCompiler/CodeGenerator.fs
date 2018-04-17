module Aqua.CodeGenerator

open Aqua.Language
open Aqua.Ast
open Aqua.Compiler
open Aqua.Bytecode

let rec compileExpr env re codeAcc expr =
    let emitBytecode = BytecodeAccumulator.appendBytecode codeAcc
    let emitExpr = compileExpr env re codeAcc

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
        | VariableLocal name ->
            emitBytecode <| LoadLocal 0
        | VariableField name ->
            ()
    | Ast_MemberAccessExpr(_, _, _) ->
        ()
    | Ast_TypeCheckExpr(child, testType) ->
        emitExpr <| child
        
        let childType = getAstExprType child

        if childType.IsReferenceType then
            emitBytecode <| CastObj(testType.ToString())
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
            emitBytecode <| CastObj(targetType.ToString())
        else
            let opcode =
                match targetType with
                | SystemTypeIdent(Bool)  -> CastBool
                | SystemTypeIdent(Int)   -> CastI32
                | SystemTypeIdent(Float) -> CastF32
                | _ -> failwith ""

            emitBytecode <| opcode
    | Ast_InvocationExpr(callee, args) ->
        args |> List.iter (fun x -> emitExpr <| x)
    | Ast_BinaryExpr(type', op, lhs, rhs) ->
        ()
