﻿module Aqua.Language

open System

type BuiltinTypeToken =
    | Ty_Unit
    | Ty_Bool
    | Ty_Int
    | Ty_Float
    | Ty_Object

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

[<RequireQualifiedAccess>]
type ControlFlow =
    | Break | Continue

[<RequireQualifiedAccess>]
type MutabilitySpec =
    | Mutable | Readonly


// Modifiers
//

[<RequireQualifiedAccess>]
type AccessModifier =
    | Private | Public

[<RequireQualifiedAccess>]
type LifetimeModifier =
    | Instance | Static

let kDefaultAccess = AccessModifier.Public
let kDefaultLifetime = LifetimeModifier.Instance

type ModifierGroup =
    { AccessType: AccessModifier
      LifetimeType: LifetimeModifier }

    static member Default =
        { AccessType = kDefaultAccess; LifetimeType = kDefaultLifetime }

module ModifierGroup =
    let getAccessType ms = ms.AccessType
    let getLifetimeType ms = ms.LifetimeType

    let isPublic ms =
        ms.AccessType = AccessModifier.Public
    let isPrivate ms =
        ms.AccessType = AccessModifier.Private

    let isInstance ms =
        ms.LifetimeType = LifetimeModifier.Instance
    let isStatic ms =
        ms.LifetimeType = LifetimeModifier.Static

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
        m |> function ModuleIdent(domain = x) -> x
    member m.Name =
        m |> function ModuleIdent(name = x) -> x

    override m.ToString() = 
        match m.Domain.Length with
        | 0 -> m.Name
        | _ -> m.Domain + "." + m.Name

// type
//
type TypeIdent =
    | DummyTypeIdent
    // primary type defined in aqua language
    | SystemTypeIdent of BuiltinTypeToken
    // TODO: classify into classes and enums
    // user-defined type
    | UserTypeIdent of ModuleIdent*string
    // aggregate function type
    | FunctionTypeIdent of FunctionSignature

    member m.IsUnitType =
        m = SystemTypeIdent(Ty_Unit)

    member m.IsBuiltinType =
        match m with
        | SystemTypeIdent(_) -> true
        | _ -> false

    member m.IsReferenceType =
        match m with
        | SystemTypeIdent(Ty_Object)
        | UserTypeIdent(_)
        | FunctionTypeIdent(_) ->
            true
        | _ ->
            false

    override m.ToString() =
        match m with
        | DummyTypeIdent ->
            "undefined"
        | SystemTypeIdent(case) ->
            match case with
            | Ty_Unit      -> "unit"
            | Ty_Bool      -> "bool"
            | Ty_Int       -> "int"
            | Ty_Float     -> "float"
            | Ty_Object    -> "object"
        | UserTypeIdent(srcModule, name) ->
            srcModule.ToString() + "::" + name
        | FunctionTypeIdent(sigature) ->
            "undefined"

and FunctionSignature =
    | FunctionSignature of TypeIdent list * TypeIdent

    member m.ParamTypeList =
        m |> function FunctionSignature(params', _) -> params'
    member m.ReturnType =
        m |> function FunctionSignature(_, ret) -> ret

    override m.ToString() =
        sprintf "(%s) -> %A" (String.Join(",", m.ParamTypeList)) (m.ReturnType)

module TypeIdent =
    let kUnitType = SystemTypeIdent Ty_Unit
    let kBoolType = SystemTypeIdent Ty_Bool
    let kIntType = SystemTypeIdent Ty_Int
    let kFloatType = SystemTypeIdent Ty_Float
    let kObjectType = SystemTypeIdent Ty_Object

    let makeUserType moduleName typeName =
        UserTypeIdent(moduleName, typeName)

    let makeFunctionType paramTypeList retType =
        FunctionTypeIdent <| FunctionSignature(paramTypeList, retType)

    let getTypeName (ident: TypeIdent) =
        ident.ToString()

    let (|OfUnitType|_|) typeIdent =
        if typeIdent = kUnitType then Some () else None
    let (|OfBoolType|_|) typeIdent =
        if typeIdent = kBoolType then Some () else None
    let (|OfIntType|_|) typeIdent =
        if typeIdent = kIntType then Some () else None
    let (|OfFloatType|_|) typeIdent =
        if typeIdent = kFloatType then Some () else None
    let (|OfObjectType|_|) typeIdent =
        if typeIdent = kObjectType then Some () else None

    let (|OfBuiltinType|_|) typeIdent =
        match typeIdent with
        | SystemTypeIdent(t) -> Some t
        | _                  -> None

// Literal
//

type Literal =
    | BoolConst of bool
    | IntConst of int

    member m.Type =
        match m with
        | BoolConst _ -> TypeIdent.kBoolType
        | IntConst _ -> TypeIdent.kIntType

// Definitions
//

type TypedName = string*TypeIdent

type MethodDefinition =
    | MethodDefinition of name: string *
                          modifiers: ModifierGroup *
                          parameters: TypedName list *
                          returnType: TypeIdent

    member m.Name =
        m |> function MethodDefinition(name = x) -> x
    member m.Modifiers =
        m |> function MethodDefinition(modifiers = x) -> x
    member m.Parameters =
        m |> function MethodDefinition(parameters = x) -> x
    member m.ReturnType =
        m |> function MethodDefinition(returnType = x) -> x


module MethodDefinition =
    let getName (def: MethodDefinition) = 
        def.Name

    let getModifiers (def: MethodDefinition) = 
        def.Modifiers

    let getParameters (def: MethodDefinition) = 
        def.Parameters

    let getReturnType (def: MethodDefinition) = 
        def.ReturnType

    let getSignature def =
        match def with
        | MethodDefinition(_, _, paramPack, retType) ->
            FunctionSignature(paramPack |> List.map snd, retType)
        

type FieldDefinition =
    | FieldDefinition of name: string *
                         modifiers: ModifierGroup *
                         mutability: MutabilitySpec *
                         type': TypeIdent

    member m.Name =
        m |> function FieldDefinition(name = x) -> x
    member m.Modifiers =
        m |> function FieldDefinition(modifiers = x) -> x
    member m.Mutability =
        m |> function FieldDefinition(mutability = x) -> x
    member m.Type =
        m |> function FieldDefinition(type' = x) -> x

module FieldDefinition =
    let getName (def: FieldDefinition) = 
        def.Name

    let getModifiers (def: FieldDefinition) = 
        def.Modifiers

    let getMutability (def: FieldDefinition) = 
        def.Mutability

    let getType (def: FieldDefinition) = 
        def.Type

    let isMutable def =
        (getMutability def) = MutabilitySpec.Mutable


type KlassDefinition =
    | KlassDefinition of name: string *
                         modifiers: ModifierGroup *
                         fields: FieldDefinition list *
                         methods: MethodDefinition list

    member m.Name =
        m |> function KlassDefinition(name = x) -> x
    member m.Modifiers =
        m |> function KlassDefinition(modifiers = x) -> x
    member m.Fields =
        m |> function KlassDefinition(fields = x) -> x
    member m.Methods =
        m |> function KlassDefinition(methods = x) -> x

module KlassDefinition =
    let getName (def: KlassDefinition) = 
        def.Name

    let getModifiers (def: KlassDefinition) = 
        def.Modifiers

    let getFields (def: KlassDefinition) = 
        def.Fields

    let getMethods (def: KlassDefinition) = 
        def.Methods


(*
type EnumItem =
    | EnumItem of name:string
type EnumDefinition =
    | EnumDefinition of name:string * values:EnumItem list

    member m.Name =
        m |> function EnumDefinition(x, _) -> x
    member m.Values =
        m |> function EnumDefinition(_, x) -> x
*)

type BasicModuleInfo =
    { ModuleName: ModuleIdent
      ImportList: ModuleIdent list
      KlassList: KlassDefinition list }