module Aqua.Language

open System

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

[<RequireQualifiedAccess>]
type MutabilitySpec =
    | Mutable | Readonly

[<RequireQualifiedAccess>]
type AccessModifier =
    | Private | Public

[<RequireQualifiedAccess>]
type LifetimeModifier =
    | Instance | Static

type ModifierGroup =
    { AccessType: AccessModifier
      LifetimeType: LifetimeModifier }

let kDefaultAccessType = AccessModifier.Public
let kDefaultLifetimeType = LifetimeModifier.Instance

// module
//
type ModuleIdent =
    // domain should be empty or period-separated identifiers
    // name should be a valid identifier
    | ModuleIdent of domain:string*name:string

    static member ofList idList =
        let a = List.toArray idList
        let domain = String.Join(".", Seq.take (a.Length-1) a)
        let name = Array.last a

        ModuleIdent(domain, name)

    member m.Domain =
        match m with | ModuleIdent(x, _) -> x
    member m.Name =
        match m with | ModuleIdent(_, x) -> x

    override m.ToString() = m.Domain + "." + m.Name

// type
//
type TypeIdent =
    // primary type defined in aqua language
    | SystemTypeIdent of BuiltinTypeCategory
    // TODO: classify into classes and enums
    // user-defined type
    | UserTypeIdent of ModuleIdent*string
    // aggregate function type
    | FunctionTypeIdent of FunctionSignature

    member m.IsReferenceType =
        match m with
        | SystemTypeIdent(Object)
        | UserTypeIdent(_)
        | FunctionTypeIdent(_) ->
            true
        | _ ->
            false

and FunctionSignature =
    | FunctionSignature of TypeIdent list * TypeIdent

    member m.ParamTypeList =
        match m with | FunctionSignature(params', _) -> params'
    member m.ReturnType =
        match m with | FunctionSignature(_, ret) -> ret

    override m.ToString() =
        sprintf "(%s) -> %A" (String.Join(",", m.ParamTypeList)) (m.ReturnType)

// shortcuts for TypeStub construction
//
let kUnitType = SystemTypeIdent Unit
let kBoolType = SystemTypeIdent Bool
let kIntType = SystemTypeIdent Int
let kFloatType = SystemTypeIdent Float
let kObjectType = SystemTypeIdent Object

let makeFunctionTypeIdent paramTypes retType =
    FunctionTypeIdent <| FunctionSignature(paramTypes, retType)

let getTypeName stub =
    match stub with
    | SystemTypeIdent(t)                 -> t.ToString()
    | UserTypeIdent(moduleName, name)    -> moduleName.ToString() + "." + name
    | FunctionTypeIdent(s)               -> s.ToString()


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

type TypedName = string*TypeIdent

type MethodDefinition =
    | MethodDefinition of name: string *
                          modifiers: ModifierGroup *
                          parameters: TypedName list *
                          returnType: TypeIdent

    member m.Name =
        match m with | MethodDefinition(x, _, _, _) -> x
    member m.Modifiers =
        match m with | MethodDefinition(_, x, _, _) -> x
    member m.Parameters =
        match m with | MethodDefinition(_, _, x, _) -> x
    member m.ReturnType =
        match m with | MethodDefinition(_, _, _, x) -> x

    member m.Signature =
        match m with
        | MethodDefinition(_, _, paramPack, retType) ->
            FunctionSignature(paramPack |> List.map snd, retType)

type FieldDefinition =
    | FieldDefinition of name: string *
                         modifiers: ModifierGroup *
                         mutability: MutabilitySpec *
                         type': TypeIdent

    member m.Name =
        match m with | FieldDefinition(x, _, _, _) -> x
    member m.Modifiers =
        match m with | FieldDefinition(_, x, _, _) -> x
    member m.Mutability =
        match m with | FieldDefinition(_, _, x, _) -> x
    member m.Type =
        match m with | FieldDefinition(_, _, _, x) -> x

type KlassDefinition =
    | KlassDefinition of name: string *
                         fields: FieldDefinition list *
                         methods: MethodDefinition list

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

type BasicModuleInfo =
    { ModuleName: ModuleIdent;
      ImportList: ModuleIdent list;
      //EnumList: EnumDefinition list;
      KlassList: KlassDefinition list; }