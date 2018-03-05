module Aqua.Language

open System
open System.Collections.Generic

open Aqua.LookupUtils

type BuiltinTypeCategory =
    | Unit
    | Bool
    | Int
    | Float
    | Object

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

type MutabilityModifier = 
    | Mutable | Readonly

type AccessModifier =
    | Private | Public

// type
//
type TypeStub =
    // primary type defined in aqua language
    | SystemStub of BuiltinTypeCategory
    // TODO: classify into classes and enums
    // user-defined type
    | UserStub of string
    // aggregate function type
    | FunctionStub of FunctionSignature

    member m.IsReferenceType =
        match m with
        | SystemStub(Object)
        | UserStub(_)
        | FunctionStub(_) ->
            true
        | _ -> 
            false

and FunctionSignature =
    | FunctionSignature of TypeStub list * TypeStub

    member m.ParamTypeList =
        match m with | FunctionSignature(params', _) -> params'
    member m.ReturnType =
        match m with | FunctionSignature(_, ret) -> ret

    override m.ToString() =
        sprintf "(%s) -> %A" (String.Join(",", m.ParamTypeList)) (m.ReturnType)

// shortcuts for TypeStub construction
//
let kUnitType = SystemStub Unit
let kBoolType = SystemStub Bool
let kIntType = SystemStub Int
let kFloatType = SystemStub Float
let kObjectType = SystemStub Object

let makeFunctionStub paramTypes retType =
    FunctionStub <| FunctionSignature(paramTypes, retType)

let getTypeName stub =
    match stub with
    | SystemStub(t)     -> t.ToString()
    | UserStub(name)    -> name
    | FunctionStub(s)   -> s.ToString()


// Literal
//

type Literal =
    | BoolConst of bool
    | IntConst of int

    member m.Type =
        match m with
        | BoolConst _ -> kBoolType
        | IntConst _ -> kIntType

// Definitions
//
type FunctionDefinition =
    | FunctionDefinition of name:string * 
                            access:AccessModifier * 
                            signature: FunctionSignature
    
    member m.Name =
        match m with | FunctionDefinition(x, _, _) -> x
    member m.Access =
        match m with | FunctionDefinition(_, x, _) -> x
    member m.Signature =
        match m with | FunctionDefinition(_, _, x) -> x

type FieldDefinition =
    | FieldDefinition of name:string * 
                         access:AccessModifier * 
                         type':TypeStub

    member m.Name =
        match m with | FieldDefinition(x, _, _) -> x
    member m.Access =
        match m with | FieldDefinition(_, x, _) -> x
    member m.Type =
        match m with | FieldDefinition(_, _, x) -> x

type KlassDefinition =
    | KlassDefinition of name:string *
                         fields:Lookup<string, FieldDefinition> * 
                         methods:Lookup<string, FunctionDefinition list>

    member m.Name =
        match m with | KlassDefinition(x, _, _) -> x
    member m.Fields =
        match m with | KlassDefinition(_, x, _) -> x
    member m.Methods =
        match m with | KlassDefinition(_, _, x) -> x

(*
type EnumItem =
    | EnumItem of name:string
type EnumDefinition =
    | EnumDefinition of name:string * values:EnumItem list

    member m.Name =
        match m with | EnumDefinition(x, _) -> x
    member m.Values =
        match m with | EnumDefinition(_, x) -> x
*)

// module
//
type ModuleIdent =
    // domain should be empty or period-separated identifiers
    // name should be a valid identifier
    | ModuleIdent of domain:string*name:string

    static member ofList l =
        let a = List.toArray l
        let domain = String.Join(".", Seq.take (a.Length-1) a)
        let name = Array.last a

        ModuleIdent(domain, name)

    member m.Domain =
        match m with | ModuleIdent(x, _) -> x
    member m.Name =
        match m with | ModuleIdent(_, x) -> x

    override m.ToString() = m.Domain + "." + m.Name

type BasicModuleInfo =
    { ModuleName: ModuleIdent;
      ImportList: ModuleIdent list;
      //EnumList: EnumDefinition list;
      KlassList: KlassDefinition list;
      FunctionList: FunctionDefinition list; }