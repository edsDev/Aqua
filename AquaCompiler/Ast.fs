module Aqua.Ast

open Aqua.Language
open FSharpx.Collections

// AstExpr
//

type AstExpr =
    | Ast_InstanceExpr      of TypeIdent
    | Ast_LiteralExpr       of Literal
    | Ast_NameAccessExpr    of TypeIdent*MutabilitySpec*VariableReference
    | Ast_MemberAccessExpr  of TypeIdent*AstExpr*string
    | Ast_TypeCheckExpr     of AstExpr*TypeIdent
    | Ast_TypeCastExpr      of AstExpr*TypeIdent
    | Ast_InvocationExpr    of CallableReference*AstExpr list
    | Ast_BinaryExpr        of TypeIdent*BinaryOp*AstExpr*AstExpr

and VariableReference =
    | VariableArgument of int
    | VariableLocal of int
    | VariableField of string

and CallableReference =
    | CallableMethod of string*MethodDefinition
    | CallableExpression of AstExpr*FunctionSignature

let rec getAstExprType expr =
    match expr with
    | Ast_InstanceExpr(t)               -> t
    | Ast_LiteralExpr(literal)          -> literal.Type
    | Ast_NameAccessExpr(t, _, _)       -> t
    | Ast_MemberAccessExpr(t, _, _)     -> t
    | Ast_TypeCheckExpr(_, _)           -> kBoolType
    | Ast_TypeCastExpr(_, t)            -> t
    | Ast_InvocationExpr(callable, _)   ->
        match callable with
        | CallableMethod(_, d)      -> d.ReturnType
        | CallableExpression(_, s)  -> s.ReturnType
    | Ast_BinaryExpr(t, _, _, _)        -> t

// AstStmt
//

type AstStmt =
    | Ast_ExpressionStmt    of AstExpr
    | Ast_VarDeclStmt       of int*TypeIdent*AstExpr
    | Ast_ChoiceStmt        of AstExpr*AstStmt*AstStmt option
    | Ast_WhileStmt         of AstExpr*AstStmt
    | Ast_ControlFlowStmt   of ControlFlow
    | Ast_ReturnStmt        of AstExpr option
    | Ast_CompoundStmt      of AstStmt list

//
//

type AstVariableDecl =
    | AstVariableDecl of string*TypeIdent*MutabilitySpec

type AstMethod =
    | AstMethod of MethodDefinition*PersistentVector<AstVariableDecl>*AstStmt

type AstKlass =
    | AstKlass of KlassDefinition*AstMethod list