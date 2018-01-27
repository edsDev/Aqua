module Aqua.Syntax

open Aqua.Language

type Identifier =
    | Identifier of string

type SyntaxType =
    | TBD
    | SystemType of BuiltinType
    | UserType of Identifier
    | FunctionType of SyntaxType list*SyntaxType

type Literal =
    | BoolConst of bool
    | IntConst of int

type Expression =
    | LiteralExpr of Literal
    | NamedExpr of Identifier
    | InvocationExpr of Expression*Expression list
    | TypeCheckExpr of Expression*SyntaxType
    | TypeConvertExpr of Expression*SyntaxType
    | BinaryExpr of BinaryOp*Expression*Expression

type Statement =
    | ExpressionStmt of Expression
    | VarDeclStmt of MutablityModifier*Identifier*SyntaxType*Expression
    | ChoiceStmt of Expression*Statement*Statement option
    | WhileStmt of Expression*Statement
    | ControlFlowStmt of ControlFlow
    | ReturnStmt of Expression option
    | CompoundStmt of Statement list

type NameTypePair =
    | NameTypePair of Identifier*SyntaxType

type FunctionDeclarator =
    | FunctionDeclarator of NameTypePair list*SyntaxType

type ModuleDecl =
    | ModuleDecl of Identifier
type ImportDecl =
    | ImportDecl of Identifier
type FunctionDecl =
    | FunctionDecl of Identifier*FunctionDeclarator*Statement
type KlassDecl =
    | KlassDecl of Identifier

type CodePage =
    { ModuleInfo  : ModuleDecl;
      Imports     : ImportDecl list;
      Functions   : FunctionDecl list;
      Klasses     : KlassDecl list }

type GlobalDeclaration =
    | GD_Function of FunctionDecl
    | GD_Klass of KlassDecl