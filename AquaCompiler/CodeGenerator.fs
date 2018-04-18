module Aqua.CodeGenerator

open Aqua.Language
open Aqua.Ast
open Aqua.Bytecode

let rec compileExpr env re codeAcc expr =
    let emitBytecode = CodeGen.appendBytecode codeAcc
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
        let childType = getAstExprType child

        if childType.IsReferenceType then
            emitExpr <| child
            
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
        emitExpr lhs
        emitExpr rhs

        match op with
        | Op_Plus ->
            emitBytecode <| Add
        | Op_Minus ->
            emitBytecode <| Sub
        | Op_Asterisk ->
            emitBytecode <| Mul
        | Op_Slash ->
            emitBytecode <| Div
        | Op_Equal ->
            emitBytecode <| Eq
        | _ ->
            failwith ""

let rec compileStmt env re codeAcc stmt =
    let emitBytecode = CodeGen.appendBytecode codeAcc
    let emitStmt = compileStmt env re codeAcc
    let emitExpr = compileExpr env re codeAcc

    match stmt with
    | Ast_ExpressionStmt(expr) ->
        emitExpr expr
        emitBytecode <| Pop
    | Ast_VarDeclStmt(mut, name, t, init) ->
        ()
    | Ast_ChoiceStmt(pred, pos, negOpt) ->
        emitExpr pred
        let patchNegJump = CodeGen.appendDummy codeAcc
        emitStmt pos

        match negOpt with
        | Some neg ->
            let patchPosJump = CodeGen.appendDummy codeAcc
            patchNegJump <| JumpOnFalse (CodeGen.extractNextIndex codeAcc)
            emitStmt neg
            patchPosJump <| Jump (CodeGen.extractNextIndex codeAcc)

        | None ->
            patchNegJump <| JumpOnFalse (CodeGen.extractNextIndex codeAcc)
            

    | Ast_WhileStmt(pred, body) ->
        let startIndex = CodeGen.extractNextIndex codeAcc

        emitExpr pred
        let patchNegJump = CodeGen.appendDummy codeAcc
        emitStmt body
        emitBytecode <| Jump startIndex

        patchNegJump <| JumpOnFalse (CodeGen.extractNextIndex codeAcc)

    | Ast_ControlFlowStmt(mode) ->
        ()
    | Ast_ReturnStmt(exprOpt) ->
        match exprOpt with
        | Some expr -> emitExpr expr
        | None -> ()

        emitBytecode <| Ret

    | Ast_CompoundStmt(children) ->
        List.iter emitStmt children