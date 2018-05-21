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

module SyntaxType =
    let getRange (syn: SyntaxType) =
        syn.Range

type SyntaxExpr =
    | Syn_InstanceExpr      of SynRange
    | Syn_LiteralExpr       of SynRange*Literal
    | Syn_NameAccessExpr    of SynRange*string
    | Syn_MemberAccessExpr  of SynRange*SyntaxExpr*string
    | Syn_InvocationExpr    of SynRange*SyntaxExpr*SyntaxExpr list
    | Syn_NewObjectExpr     of SynRange*SyntaxType*SyntaxExpr list
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
        | Syn_NewObjectExpr(rg, _, _)       -> rg
        | Syn_TypeCheckExpr(rg, _, _)       -> rg
        | Syn_TypeCastExpr(rg, _, _)        -> rg
        | Syn_BinaryExpr(rg, _, _, _)       -> rg

module SyntaxExpr =
    let getRange (syn: SyntaxExpr) =
        syn.Range

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

module SyntaxStmt =
    let getRange (syn: SyntaxStmt) =
        syn.Range

type MethodDeclarator =
    | MethodDeclarator of (string*SyntaxType) list*SyntaxType

    member m.ParamList =
        m |> function MethodDeclarator(ps, _) -> ps
    member m.ReturnType =
        m |> function MethodDeclarator(_, ret) -> ret

type MethodDecl =
    | MethodDecl of string*ModifierGroup*MethodDeclarator*SyntaxStmt

    member m.Name =
        m |> function MethodDecl(x, _, _, _) -> x
    member m.Modifiers =
        m |> function MethodDecl(_, x, _, _) -> x
    member m.Declarator =
        m |> function MethodDecl(_, _, x, _) -> x
    member m.Body =
        m |> function MethodDecl(_, _, _, x) -> x

type FieldDecl =
    | FieldDecl of string*ModifierGroup*MutabilitySpec*SyntaxType

    member m.Name =
        m |> function FieldDecl(x, _, _, _) -> x
    member m.Modifiers =
        m |> function FieldDecl(_, x, _, _) -> x
    member m.Mutability =
        m |> function FieldDecl(_, _, x, _) -> x
    member m.Type =
        m |> function FieldDecl(_, _, _, x) -> x

type KlassDecl =
    | KlassDecl of string*ModifierGroup*MethodDecl list*FieldDecl list

    member m.Name =
        m |> function KlassDecl(x, _, _, _) -> x
    member m.Modifiers =
        m |> function KlassDecl(_, x, _, _) -> x
    member m.Methods =
        m |> function KlassDecl(_, _, x, _) -> x
    member m.Fields =
        m |> function KlassDecl(_, _, _, x) -> x

type ModuleDecl =
    | ModuleDecl of SynRange*ModuleIdent

    member m.Range =
        m |> function ModuleDecl(x, _) -> x
    member m.Identifier =
        m |> function ModuleDecl(_, x) -> x

type ImportDecl =
    | ImportDecl of SynRange*ModuleIdent

    member m.Range =
        m |> function ImportDecl(x, _) -> x
    member m.Identifier =
        m |> function ImportDecl(_, x) -> x

type SyntaxCodePage =
    { Path        : string
      ModuleInfo  : ModuleDecl
      ImportList  : ImportDecl list
      KlassList   : KlassDecl list }
