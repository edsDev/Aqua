module Aqua.Syntax

open Aqua.Language

type SynRange =
    { StartIndex: int
      Length: int
      StartLine: int
      StartColumn: int }

    static member Empty =
        { StartIndex = -1; Length = -1; StartLine = -1; StartColumn = -1 }

type SyntaxType =
    | Syn_SystemType    of SynRange*BuiltinType
    | Syn_UserType      of SynRange*string
    | Syn_FunctionType  of SynRange*SyntaxType list*SyntaxType

    member m.Range =
        match m with
        | Syn_SystemType(rg, _)         -> rg
        | Syn_UserType(rg, _)           -> rg
        | Syn_FunctionType(rg, _, _)    -> rg

type SyntaxExpr =
    | Syn_InstanceExpr      of SynRange
    | Syn_LiteralExpr       of SynRange*Literal
    | Syn_NameAccessExpr    of SynRange*string
    | Syn_MemberAccessExpr  of SynRange*SyntaxExpr*string
    | Syn_InvocationExpr    of SynRange*SyntaxExpr*SyntaxExpr list
    | Syn_TypeCheckExpr     of SynRange*SyntaxExpr*SyntaxType
    | Syn_TypeCastExpr      of SynRange*SyntaxExpr*SyntaxType
    | Syn_BinaryExpr        of SynRange*BinaryOp*SyntaxExpr*SyntaxExpr

    member m.Range =
        match m with
        | Syn_InstanceExpr(rg)              -> rg
        | Syn_LiteralExpr(rg, _)            -> rg
        | Syn_NameAccessExpr(rg, _)         -> rg
        | Syn_MemberAccessExpr(rg, _, _)    -> rg
        | Syn_InvocationExpr(rg, _, _)      -> rg
        | Syn_TypeCheckExpr(rg, _, _)       -> rg
        | Syn_TypeCastExpr(rg, _, _)        -> rg
        | Syn_BinaryExpr(rg, _, _, _)       -> rg

type SyntaxStmt =
    | Syn_ExpressionStmt    of SynRange*SyntaxExpr
    | Syn_VarDeclStmt       of SynRange*MutabilitySpec*string*SyntaxType option*SyntaxExpr
    | Syn_ChoiceStmt        of SynRange*SyntaxExpr*SyntaxStmt*SyntaxStmt option
    | Syn_WhileStmt         of SynRange*SyntaxExpr*SyntaxStmt
    | Syn_ControlFlowStmt   of SynRange*ControlFlow
    | Syn_ReturnStmt        of SynRange*SyntaxExpr option
    | Syn_CompoundStmt      of SynRange*SyntaxStmt list

    member m.Range =
        match m with
        | Syn_ExpressionStmt(rg, _)         -> rg
        | Syn_VarDeclStmt(rg, _, _, _, _)   -> rg
        | Syn_ChoiceStmt(rg, _, _, _)       -> rg
        | Syn_WhileStmt(rg, _, _)           -> rg
        | Syn_ControlFlowStmt(rg, _)        -> rg
        | Syn_ReturnStmt(rg, _)             -> rg
        | Syn_CompoundStmt(rg, _)           -> rg

type MethodDeclarator =
    | MethodDeclarator of (string*SyntaxType) list*SyntaxType

    member m.ParamList =
        match m with | MethodDeclarator(ps, _) -> ps
    member m.ReturnType =
        match m with | MethodDeclarator(_, ret) -> ret

type MethodDecl =
    | MethodDecl of string*ModifierGroup*MethodDeclarator*SyntaxStmt

    member m.Name =
        match m with | MethodDecl(x, _, _, _) -> x
    member m.Modifiers =
        match m with | MethodDecl(_, x, _, _) -> x
    member m.Declarator =
        match m with | MethodDecl(_, _, x, _) -> x
    member m.Body =
        match m with | MethodDecl(_, _, _, x) -> x

type FieldDecl =
    | FieldDecl of string*ModifierGroup*MutabilitySpec*SyntaxType

    member m.Name =
        match m with | FieldDecl(x, _, _, _) -> x
    member m.Modifiers =
        match m with | FieldDecl(_, x, _, _) -> x
    member m.Mutability =
        match m with | FieldDecl(_, _, x, _) -> x
    member m.Type =
        match m with | FieldDecl(_, _, _, x) -> x

type KlassDecl =
    | KlassDecl of string*ModifierGroup*MethodDecl list*FieldDecl list

    member m.Name =
        match m with | KlassDecl(x, _, _, _) -> x
    member m.Modifiers =
        match m with | KlassDecl(_, x, _, _) -> x
    member m.Methods =
        match m with | KlassDecl(_, _, x, _) -> x
    member m.Fields =
        match m with | KlassDecl(_, _, _, x) -> x

type ModuleDecl =
    | ModuleDecl of SynRange*ModuleIdent

    member m.Range =
        match m with | ModuleDecl(x, _) -> x
    member m.Identifier =
        match m with | ModuleDecl(_, x) -> x

type ImportDecl =
    | ImportDecl of SynRange*ModuleIdent

    member m.Range =
        match m with | ImportDecl(x, _) -> x
    member m.Identifier =
        match m with | ImportDecl(_, x) -> x

type SyntaxCodePage =
    { Path        : string
      ModuleInfo  : ModuleDecl
      ImportList  : ImportDecl list
      KlassList   : KlassDecl list }
