module Aqua.Ast

open Aqua.Language

type AstExpr =
    | Ast_LiteralExpr       of Literal
    | Ast_NameAccessExpr         of TypeStub*MutablityModifier*VariableReference
    | Ast_MemberAccessExpr  of TypeStub*AstExpr*string
    | Ast_TypeCheckExpr     of AstExpr*TypeStub
    | Ast_TypeCastExpr      of AstExpr*TypeStub
    | Ast_InvocationExpr    of CallableReference*AstExpr list
    | Ast_BinaryExpr        of TypeStub*BinaryOp*AstExpr*AstExpr

and VariableReference =
    | VariableLocal of string
    | VariableField of string

and CallableReference =
    | CallableFunction of FunctionDefinition
    | CallableMethod of string*FunctionDefinition
    | CallableExpression of AstExpr*FunctionSignature

module AstExpr =
    let rec getTypeStub expr =
        match expr with
        | Ast_LiteralExpr(literal)          -> literal.Type
        | Ast_NameAccessExpr(t, _, _)       -> t
        | Ast_MemberAccessExpr(t, _, _)     -> t
        | Ast_TypeCheckExpr(_, t)           -> t
        | Ast_TypeCastExpr(_, t)            -> t
        | Ast_InvocationExpr(callable, _)   ->
            match callable with
            | CallableFunction(d)       -> d.Signature.ReturnType
            | CallableMethod(_, d)      -> d.Signature.ReturnType
            | CallableExpression(_, s)  -> s.ReturnType
        | Ast_BinaryExpr(t, _, _, _)        -> t

type AstStmt =
    | Ast_ExpressionStmt    of AstExpr
    | Ast_VarDeclStmt       of MutablityModifier*string*TypeStub*AstExpr
    | Ast_ChoiceStmt        of AstExpr*AstStmt*AstStmt option
    | Ast_WhileStmt         of AstExpr*AstStmt
    | Ast_ControlFlowStmt   of ControlFlow
    | Ast_ReturnStmt        of AstExpr option
    | Ast_CompoundStmt      of AstStmt list