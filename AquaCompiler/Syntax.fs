module Aqua.Syntax

open Aqua.Language

type Range =
    { StartIndex: int; Length: int; StartLine: int; StartColumn: int }

    static member Empty =
        { StartIndex = -1; Length = -1; StartLine = -1; StartColumn = -1 }

type SyntaxType =
    | Syn_SystemType    of Range*BuiltinTypeCategory
    | Syn_UserType      of Range*string
    | Syn_FunctionType  of Range*SyntaxType list*SyntaxType

    member m.Range =
        match m with 
        | Syn_SystemType(rg, _)         -> rg
        | Syn_UserType(rg, _)           -> rg
        | Syn_FunctionType(rg, _, _)    -> rg

type SyntaxExpr =
    | Syn_InstanceExpr      of Range
    | Syn_LiteralExpr       of Range*Literal
    | Syn_NameAccessExpr    of Range*string
    | Syn_MemberAccessExpr  of Range*SyntaxExpr*string
    | Syn_InvocationExpr    of Range*SyntaxExpr*SyntaxExpr list
    | Syn_TypeCheckExpr     of Range*SyntaxExpr*SyntaxType
    | Syn_TypeCastExpr      of Range*SyntaxExpr*SyntaxType
    | Syn_BinaryExpr        of Range*BinaryOp*SyntaxExpr*SyntaxExpr

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
    | Syn_ExpressionStmt    of Range*SyntaxExpr
    | Syn_VarDeclStmt       of Range*MutabilityModifier*string*SyntaxType option*SyntaxExpr
    | Syn_ChoiceStmt        of Range*SyntaxExpr*SyntaxStmt*SyntaxStmt option
    | Syn_WhileStmt         of Range*SyntaxExpr*SyntaxStmt
    | Syn_ControlFlowStmt   of Range*ControlFlow
    | Syn_ReturnStmt        of Range*SyntaxExpr option
    | Syn_CompoundStmt      of Range*SyntaxStmt list

    member m.Range =
        match m with
        | Syn_ExpressionStmt(rg, _)         -> rg
        | Syn_VarDeclStmt(rg, _, _, _, _)   -> rg
        | Syn_ChoiceStmt(rg, _, _, _)       -> rg
        | Syn_WhileStmt(rg, _, _)           -> rg
        | Syn_ControlFlowStmt(rg, _)        -> rg
        | Syn_ReturnStmt(rg, _)             -> rg
        | Syn_CompoundStmt(rg, _)           -> rg

type FunctionDeclarator =
    | FunctionDeclarator of (string*SyntaxType) list*SyntaxType

    member m.ParamList =
        match m with | FunctionDeclarator(ps, _) -> ps
    member m.ReturnType =
        match m with | FunctionDeclarator(_, ret) -> ret

type FunctionDecl =
    | FunctionDecl of string*AccessModifier*FunctionDeclarator*SyntaxStmt

    member m.Name =
        match m with | FunctionDecl(x, _, _, _) -> x
    member m.Access =
        match m with | FunctionDecl(_, x, _, _) -> x
    member m.Declarator =
        match m with | FunctionDecl(_, _, x, _) -> x
    member m.Body =
        match m with | FunctionDecl(_, _, _, x) -> x

type FieldDecl =
    | FieldDecl of string*AccessModifier*MutabilityModifier*SyntaxType

    member m.Name =
        match m with | FieldDecl(x, _, _, _) -> x
    member m.Access =
        match m with | FieldDecl(_, x, _, _) -> x
    member m.Mutability =
        match m with | FieldDecl(_, _, x, _) -> x
    member m.Body =
        match m with | FieldDecl(_, _, _, x) -> x

type KlassDecl =
    | KlassDecl of string*AccessModifier*FunctionDecl list*FieldDecl list

    member m.Name =
        match m with | KlassDecl(x, _, _, _) -> x
    member m.Access =
        match m with | KlassDecl(_, x, _, _) -> x
    member m.Methods =
        match m with | KlassDecl(_, _, x, _) -> x
    member m.Fields =
        match m with | KlassDecl(_, _, _, x) -> x

type ModuleDecl =
    | ModuleDecl of Range*ModuleIdent
type ImportDecl =
    | ImportDecl of Range*ModuleIdent

type CodePage =
    { ModuleName  : ModuleIdent;
      ImportList  : ModuleIdent list;
      Functions   : FunctionDecl list;
      Klasses     : KlassDecl list }
