module Aqua.Syntax

open Aqua.Language

type Range =
    { StartIndex: int; Length: int; StartLine: int; StartColumn: int }

    static member Empty =
        { StartIndex = -1; Length = -1; StartLine = -1; StartColumn = -1 }

type Literal =
    | BoolConst of bool
    | IntConst of int

type Expression =
    | LiteralExpr       of Range*Literal
    | NamedExpr         of Range*string
    | InvocationExpr    of Range*Expression*Expression list
    | TypeCheckExpr     of Range*Expression*TypeStub
    | TypeCastExpr      of Range*Expression*TypeStub
    | BinaryExpr        of Range*BinaryOp*Expression*Expression

    member m.Range =
        match m with
        | LiteralExpr(rg, _)        -> rg
        | NamedExpr(rg, _)          -> rg
        | InvocationExpr(rg, _, _)  -> rg
        | TypeCheckExpr(rg, _, _)   -> rg
        | TypeCastExpr(rg, _, _)    -> rg
        | BinaryExpr(rg, _, _, _)   -> rg

type Statement =
    | ExpressionStmt    of Range*Expression
    | VarDeclStmt       of Range*MutablityModifier*string*TypeStub option*Expression
    | ChoiceStmt        of Range*Expression*Statement*Statement option
    | WhileStmt         of Range*Expression*Statement
    | ControlFlowStmt   of Range*ControlFlow
    | ReturnStmt        of Range*Expression option
    | CompoundStmt      of Range*Statement list

    member m.Range =
        match m with
        | ExpressionStmt(rg, _)         -> rg
        | VarDeclStmt(rg, _, _, _, _)   -> rg
        | ChoiceStmt(rg, _, _, _)       -> rg
        | WhileStmt(rg, _, _)           -> rg
        | ControlFlowStmt(rg, _)        -> rg
        | ReturnStmt(rg, _)             -> rg
        | CompoundStmt(rg, _)           -> rg

type FunctionDeclarator =
    | FunctionDeclarator of (string*TypeStub) list*TypeStub

    member m.ParamList =
        match m with | FunctionDeclarator(ps, _) -> ps

    member m.ReturnType =
        match m with | FunctionDeclarator(_, ret) -> ret

type ModuleDecl =
    | ModuleDecl of ModuleIdent

    member m.Name =
        match m with | ModuleDecl(name) -> name

type ImportDecl =
    | ImportDecl of ModuleIdent

    member m.Name =
        match m with | ImportDecl(name) -> name

type FunctionDecl =
    | FunctionDecl of string*FunctionDeclarator*Statement

    member m.Name =
        match m with | FunctionDecl(name, _, _) -> name

    member m.Declarator =
        match m with | FunctionDecl(_, d, _) -> d

    member m.Body =
        match m with | FunctionDecl(_, _, body) -> body

type KlassDecl =
    | KlassDecl of string

    member m.Name =
        match m with | KlassDecl(name) -> name

type CodePage =
    { ModuleInfo  : ModuleDecl;
      Imports     : ImportDecl list;
      Functions   : FunctionDecl list;
      Klasses     : KlassDecl list }

type GlobalDeclaration =
    | GD_Function of FunctionDecl
    | GD_Klass of KlassDecl