module Aqua.CodeGenerator

open Aqua.Language
open Aqua.Ast
open Aqua.Compiler
open Aqua.Bytecode

(*
let rec translateExpr env re codeAcc expr =
    let emitBytecode = BytecodeAccumulator.appendBytecode codeAcc
    let emitExpr = translateExpr env re codeAcc

    match expr with
    | Ast_LiteralExpr(literal) ->
        match literal with
        | BoolConst(x) ->
            emitBytecode <| PushI32(if x then 1 else 0)
        | IntConst(x) ->
            emitBytecode <| PushI32(x)
    | Ast_NamedExpr(_, h) ->
        ()
    | Ast_TypeCheckExpr(child, testType) ->
        emitExpr <| child
        
        if child.Type.IsReferenceType then
            emitBytecode <| CastObj(testType.ToString())
            emitBytecode <| CastBool
        else
            match child.Type, testType with
            | SystemStub(c1), SystemStub(c2) ->
                emitBytecode <| PushI32(if c1=c2 then 1 else 0)
            | _ ->
                emitBytecode <| PushI32(0)
    | Ast_TypeCastExpr(child, targetType) ->
        emitExpr <| child

        if child.Type.IsReferenceType then
            emitBytecode <| CastObj(targetType.ToString())
        else
            let opcode =
                match targetType with
                | SystemStub(Bool)  -> CastBool
                | SystemStub(Int)   -> CastI32
                | SystemStub(Float) -> CastF32
                | _ -> failwith ""

            emitBytecode <| opcode
    | Ast_InvocationExpr(callee, args) ->
        args |> List.iter (fun x -> emitExpr <| x)

*)