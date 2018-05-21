module Aqua.CodeGenerator

open Aqua.Language
open Aqua.Ast
open Aqua.Bytecode
open CollectionUtils

type CodeGenContext =
    { LoopStart: int 
      LoopBreakCallback: (Bytecode -> unit) list }

module CodeGenContext =
    let createContext loopStart =
        { LoopStart = loopStart 
          LoopBreakCallback = [] }

    let appendBreakCallback callback ctx =
        { ctx with LoopBreakCallback = callback::ctx.LoopBreakCallback }

let rec compileExpr codeBuf expr =
    let emitBytecode = CodeGen.appendBytecode codeBuf
    let emitExpr = compileExpr codeBuf

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

    | Ast_MemberAccessExpr(object, fieldRef) ->
        emitExpr <| object
        emitBytecode <| LoadField fieldRef

    | Ast_TypeCheckExpr(expr, testType) ->
        if expr.Type.IsReferenceType then
            emitExpr <| expr
            
            emitBytecode <| CastObj testType
            emitBytecode <| CastBool
        else
            match expr.Type, testType with
            | TypeIdent.OfBuiltinType(c1), TypeIdent.OfBuiltinType(c2) ->
                emitBytecode <| PushI32 (if c1=c2 then 1 else 0)
            | _ ->
                emitBytecode <| PushI32 0

    | Ast_TypeCastExpr(expr, targetType) ->
        emitExpr <| expr

        if expr.Type.IsReferenceType then
            emitBytecode <| CastObj targetType
        else
            let opcode =
                match targetType with
                | TypeIdent.OfBoolType  -> CastBool
                | TypeIdent.OfIntType   -> CastI32
                | TypeIdent.OfFloatType -> CastF32
                | _ -> failwith "not implemented"

            emitBytecode <| opcode

    | Ast_InvocationExpr(callee, args) ->
        args |> List.iter emitExpr
        match callee with
        | CallableInstanceMethod(instance, methodRef) ->
            // TODO: this is actually incorrect
            //       overloading is not considered yet
            emitExpr <| instance
            emitBytecode <| Call methodRef

        | CallableStaticMethod(methodRef) ->
            emitBytecode <| Call methodRef

        | CallableExpression(calledExpr, sigature) ->
            failwith "expression call not supported yet"

    | Ast_NewObjectExpr(ctor, args) ->
        args |> List.iter emitExpr
        emitBytecode <| NewObj ctor

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
                emitBytecode <| StoreField field

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
let rec compileStmt codeBuf ctx stmt =
    let emitBytecode = CodeGen.appendBytecode codeBuf
    let emitExpr = compileExpr codeBuf

    match stmt with
    | Ast_ExpressionStmt(expr) ->
        emitExpr expr

        if not <| (AstExpr.getType expr).IsUnitType then
            emitBytecode <| Pop

        ctx

    | Ast_VarDeclStmt(id, t, init) ->
        emitExpr init
        emitBytecode <| StoreLocal id

        ctx

    | Ast_ChoiceStmt(pred, pos, negOpt) ->
        emitExpr pred
        let patchNegJump = CodeGen.appendDummy codeBuf
        let ctx1 = compileStmt codeBuf ctx pos

        match negOpt with
        | Some neg ->
            let patchPosJump = CodeGen.appendDummy codeBuf
            patchNegJump <| JumpOnFalse (CodeGen.extractNextIndex codeBuf)
            let ctx2 = compileStmt codeBuf ctx1 neg
            patchPosJump <| Jump (CodeGen.extractNextIndex codeBuf)

            ctx2

        | None ->
            patchNegJump <| JumpOnFalse (CodeGen.extractNextIndex codeBuf)
            
            ctx1

    | Ast_WhileStmt(pred, body) ->
        let loopStart = CodeGen.extractNextIndex codeBuf

        emitExpr pred
        let patchNegJump = CodeGen.appendDummy codeBuf
        let ctx' = compileStmt codeBuf (CodeGenContext.createContext loopStart) body
        emitBytecode <| Jump loopStart

        let loopEnd = (CodeGen.extractNextIndex codeBuf)
        
        patchNegJump <| JumpOnFalse loopEnd

        ctx'.LoopBreakCallback
        |> List.iter (fun patch -> patch (Jump loopEnd))

        ctx

    | Ast_ControlFlowStmt(mode) ->
        match mode with
        | ControlFlow.Break -> 
            ctx |> CodeGenContext.appendBreakCallback (CodeGen.appendDummy codeBuf)

        | ControlFlow.Continue ->
            emitBytecode <| Jump ctx.LoopStart
            ctx

    | Ast_ReturnStmt(exprOpt) ->
        match exprOpt with
        | Some expr -> emitExpr expr
        | None -> ()

        emitBytecode <| Ret

        ctx

    | Ast_CompoundStmt(children) ->
        children 
        |> List.fold (compileStmt codeBuf) ctx

let compileModule = ()