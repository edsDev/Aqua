module Aqua.Language

open System
open System.Collections.Generic

type BuiltinType =
    | Unit
    | Bool
    | Int

type BinaryOp =
    | Op_Plus
    | Op_Minus
    | Op_Asterisk
    | Op_Slash
    | Op_Modulus
    | Op_Greater
    | Op_GreaterEq
    | Op_Less
    | Op_LessEq
    | Op_Equal
    | Op_NotEqual
    | Op_BitwiseAnd
    | Op_BitwiseOr
    | Op_BitwiseXor
    | Op_LogicalAnd
    | Op_LogicalOr

type ControlFlow = 
    | Break | Continue

type MutablityModifier = 
    | Mutable | Readonly

type AccessModifier =
    | Private | Public

// type
type TypeStub =
    | SystemTypeStub of BuiltinType
    | UserTypeStub of string
    | FunctionTypeStub of FunctionSignature
with
    override m.ToString() =
        match m with
        | SystemTypeStub(t) -> t.ToString()
        | UserTypeStub(name) -> name
        | FunctionTypeStub(s) -> s.ToString()
and FunctionSignature =
    | FunctionSignature of TypeStub list * TypeStub
with
    member m.ParamTypeList =
        match m with | FunctionSignature(params', _) -> params'
    member m.ReturnType =
        match m with | FunctionSignature(_, ret) -> ret

    override m.ToString() =
        sprintf "(%s) -> %A" (String.Join(",", m.ParamTypeList)) (m.ReturnType)

// shortcuts for system type
let UnitType = SystemTypeStub Unit
let BoolType = SystemTypeStub Bool
let IntType = SystemTypeStub Int

// definitions
type FunctionDefinition =
    | FunctionDefinition of name:string * signature: FunctionSignature

type EnumItem =
    | EnumItem of name:string
type EnumDefinition =
    | EnumDefinition of name:string * values:EnumItem list

type KlassField =
    | KlassField of name:string * access:AccessModifier * type':TypeStub
type KlassMethod =
    | KlassMethod of name:string * access:AccessModifier * signature:FunctionSignature
type KlassDefinition =
    | KlassDefinition of name:string * fields:KlassField list * methods:KlassMethod list

// module
//
type ModuleId =
    | ModuleId of IReadOnlyList<string>
with 
    member m.NameList = 
        match m with | ModuleId(x) -> x

    override m.ToString() = String.Join(".", m.NameList)

type BasicModuleInfo =
    { ModuleName: ModuleId;
      ImportList: ModuleId list;
      EnumList: EnumDefinition list;
      KlassList: KlassDefinition list;
      FunctionList: FunctionDefinition list; }