module Aqua.Ast

open Aqua.Language
open FSharpx.Collections

// reference
//

type KlassReference =
    | KlassReference of ModuleIdent*KlassDefinition

    member m.Module =
        m |> function KlassReference(x, _) -> x
    member m.Definition =
        m |> function KlassReference(_, x) -> x

    member m.Type =
        UserTypeIdent(m.Module, m.Definition.Name)

type MethodReference =
    | MethodReference of ModuleIdent*KlassDefinition*MethodDefinition

    member m.Module =
        m |> function MethodReference(x, _, _) -> x
    member m.Klass =
        m |> function MethodReference(_, x, _) -> x
    member m.Definition =
        m |> function MethodReference(_, _, x) -> x

type FieldReference =
    | FieldReference of ModuleIdent*KlassDefinition*FieldDefinition

    member m.Module =
        m |> function FieldReference(x, _, _) -> x
    member m.Klass =
        m |> function FieldReference(_, x, _) -> x
    member m.Definition =
        m |> function FieldReference(_, _, x) -> x

// AstExpr
//

type AstExpr =
    | Ast_InstanceExpr      of TypeIdent
    | Ast_LiteralExpr       of Literal
    | Ast_NameAccessExpr    of TypeIdent*VariableReference
    | Ast_MemberAccessExpr  of AstExpr*FieldReference
    | Ast_TypeCheckExpr     of AstExpr*TypeIdent
    | Ast_TypeCastExpr      of AstExpr*TypeIdent
    | Ast_InvocationExpr    of CallableReference*AstExpr list
    | Ast_BinaryExpr        of TypeIdent*BinaryOp*AstExpr*AstExpr

    member m.Type =
        match m with
        | Ast_InstanceExpr(t)               -> t
        | Ast_LiteralExpr(literal)          -> literal.Type
        | Ast_NameAccessExpr(t, _)          -> t
        | Ast_MemberAccessExpr(_, ref)      -> ref.Definition.Type
        | Ast_TypeCheckExpr(_, _)           -> kBoolType
        | Ast_TypeCastExpr(_, t)            -> t
        | Ast_BinaryExpr(t, _, _, _)        -> t
        | Ast_InvocationExpr(callable, _)   ->
            match callable with
            | CallableInstanceMethod(_, r)
                -> r.Definition.ReturnType
            | CallableStaticMethod(r)
                -> r.Definition.ReturnType
            | CallableExpression(_, s)
                -> s.ReturnType

and VariableReference =
    | VariableArgument of int
    | VariableLocal of int

and CallableReference =
    | CallableInstanceMethod of AstExpr*MethodReference
    | CallableStaticMethod of MethodReference
    | CallableExpression of AstExpr*FunctionSignature

module AstExpr =
    let getType (expr: AstExpr) =
        expr.Type

    let createInstance typeAnnot =
        Ast_InstanceExpr typeAnnot

    let createLiteral literal =
        Ast_LiteralExpr literal

    let createArgumentAccess typeAnnot id =
        Ast_NameAccessExpr (typeAnnot, VariableArgument id)

    let createLocalAccess typeAnnot id =
        Ast_NameAccessExpr (typeAnnot, VariableLocal id)

    
    let createInvokeInstance thisExpr method argExprList =
        Ast_InvocationExpr (CallableInstanceMethod (thisExpr, method), argExprList)

    let createInvokeStatic method argExprList =
        Ast_InvocationExpr (CallableStaticMethod method, argExprList)

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