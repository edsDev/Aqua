module Aqua.Language

open System
open System.Collections.Generic

type BuiltinTypeCategory =
    | Unit
    | Bool
    | Int

type BinaryOp =
    | Op_Assign

    | Op_Plus
    | Op_Minus
    | Op_Asterisk
    | Op_Slash
    | Op_Modulus

    | Op_Equal
    | Op_NotEqual
    | Op_Greater
    | Op_GreaterEq
    | Op_Less
    | Op_LessEq

    | Op_BitwiseAnd
    | Op_BitwiseOr
    | Op_BitwiseXor
    | Op_Conjunction
    | Op_Disjunction

type ControlFlow = 
    | Break | Continue

type MutablityModifier = 
    | Mutable | Readonly

type AccessModifier =
    | Private | Public

// type
//
type TypeStub =
    | WildcardStub // a internal placeholder type
    | SystemStub of BuiltinTypeCategory
    | UserStub of string
    | FunctionStub of FunctionSignature
with
    override m.ToString() =
        match m with
        | WildcardStub -> "WILDCARD"
        | SystemStub(t) -> t.ToString()
        | UserStub(name) -> name
        | FunctionStub(s) -> s.ToString()
and FunctionSignature =
    | FunctionSignature of TypeStub list * TypeStub
with
    member m.ParamTypeList =
        match m with | FunctionSignature(params', _) -> params'
    member m.ReturnType =
        match m with | FunctionSignature(_, ret) -> ret

    override m.ToString() =
        sprintf "(%s) -> %A" (String.Join(",", m.ParamTypeList)) (m.ReturnType)

// shortcuts for TypeStub construction
let kUnitType = SystemStub Unit
let kBoolType = SystemStub Bool
let kIntType = SystemStub Int

let makeFunctionStub paramTypes retType =
    FunctionStub <| FunctionSignature(paramTypes, retType)

// definitions
//
type FunctionDefinition =
    | FunctionDefinition of name:string * access:AccessModifier * signature: FunctionSignature
with
    member m.Name =
        match m with | FunctionDefinition(x, _, _) -> x
    member m.Access =
        match m with | FunctionDefinition(_, x, _) -> x
    member m.Signature =
        match m with | FunctionDefinition(_, _, x) -> x

type EnumItem =
    | EnumItem of name:string
type EnumDefinition =
    | EnumDefinition of name:string * values:EnumItem list
with
    member m.Name =
        match m with | EnumDefinition(x, _) -> x
    member m.Values =
        match m with | EnumDefinition(_, x) -> x

type KlassField =
    | KlassField of name:string * access:AccessModifier * type':TypeStub
type KlassDefinition =
    | KlassDefinition of name:string * fields:KlassField list * methods:FunctionDefinition list
with
    member m.Name =
        match m with | KlassDefinition(x, _, _) -> x
    member m.Fields =
        match m with | KlassDefinition(_, x, _) -> x
    member m.Methods =
        match m with | KlassDefinition(_, _, x) -> x

// module
//
type ModuleIdent =
    | ModuleIdent of IReadOnlyList<string>
with 
    member m.NameList = 
        match m with | ModuleIdent(x) -> x

    member m.TerminalName =
        m.NameList.Item (m.NameList.Count-1)

    override m.ToString() = String.Join(".", m.NameList)

type BasicModuleInfo =
    { ModuleName: ModuleIdent;
      ImportList: ModuleIdent list;
      EnumList: EnumDefinition list;
      KlassList: KlassDefinition list;
      FunctionList: FunctionDefinition list; }