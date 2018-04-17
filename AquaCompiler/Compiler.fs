module Aqua.Compiler

open Aqua.LookupUtils
open Aqua.Language
open Aqua.Syntax
open Aqua.Ast
open System.IO
open System.Collections.Generic

type ErrorMessage =
    { ReferenceRange: SynRange; Message: string; }

type ErrorMessageList = ErrorMessage list

//
// Translation Session
//
let parseModuleInfo s = failwith ""

type ModuleLoader(importPathList) =
    let importCache = Dictionary<ModuleIdent, BasicModuleInfo>()

    do for path in importPathList do
        if not <| Directory.Exists(path) then
            failwith <| sprintf "import path %s does not exist" path

    member m.LoadModule ident =
        if importCache.ContainsKey(ident) then
            Some <| importCache.[ident]
        else
            let moduleInfo =
                importPathList
                |> List.tryPick (fun dirPath ->
                                     let filePath = dirPath + ident.Domain + "." + ident.Name + ".am"

                                     if File.Exists(filePath) then
                                        Some <| File.ReadAllText(filePath)
                                     else
                                        None)
                |> Option.map parseModuleInfo

            if moduleInfo.IsSome then
                importCache.Add(ident, moduleInfo.Value)

            moduleInfo

//
// Translation Environment
//

// <field name> -> <definition>
type FieldLookupTable = Lookup<string, FieldDefinition>
// <function name> -> <definition list>
type MethodLookupTable = Lookup<string, MethodDefinition list>

type TypeLookupItem =
    //| EnumLookupItem of EnumDefinition
    | KlassLookupItem of ModuleIdent*KlassDefinition*FieldLookupTable*MethodLookupTable

    member m.Type =
        match m with
        | KlassLookupItem(moduleName, def, _, _) -> UserTypeIdent(moduleName, def.Name)

// <type name> -> <definition>
type TypeLookupTable = Lookup<string, TypeLookupItem>

type VariableLookupItem =
    | VariableLookupItem of string*MutabilitySpec*TypeIdent

    member m.Name =
        match m with | VariableLookupItem(x, _, _) -> x
    member m.Mutability =
        match m with | VariableLookupItem(_, x, _) -> x
    member m.Type =
        match m with | VariableLookupItem(_, _, x) -> x

type TranslationEnvironment =
    { CurrentModule: ModuleIdent
      CurrentKlass: KlassDefinition
      CurrentMethod: MethodDefinition

      TypeLookup: TypeLookupTable }

type TranslationContext =
    { Environment: TranslationEnvironment

      LoopDepth: int
      VariableLookup: Map<string, VariableLookupItem>
      ErrorMessages: ErrorMessageList }

let createContext env =
    let varLookup =
        env.CurrentMethod.Parameters
        |> Seq.map (fun (name, t) -> name, VariableLookupItem(name, MutabilitySpec.Readonly, t))
        |> Map.ofSeq

    { Environment = env
      LoopDepth = 0
      VariableLookup = varLookup
      ErrorMessages = [] }

// context proxy
//

let getCurrentKlass ctx =
    ctx.Environment.CurrentKlass

let getCurrentMethod ctx =
    ctx.Environment.CurrentMethod

let getCurrentKlassType ctx =
    let env = ctx.Environment
    UserTypeIdent(env.CurrentModule, env.CurrentKlass.Name)

let getMethodName ctx =
    (getCurrentMethod ctx).Name

let getReturnType ctx =
    (getCurrentMethod ctx).Signature.ReturnType

let isInstanceContext ctx =
    (getCurrentMethod ctx).Modifiers.LifetimeType <> LifetimeModifier.Static

let lookupType ctx name =
     ctx.Environment.TypeLookup
     |> Lookup.tryFind name

let lookupMethod ctx typeName funcName =
    lookupType ctx typeName
    |> Option.bind (function | KlassLookupItem(_, _, _, table) -> table |> Lookup.tryFind funcName)
    |> Option.defaultValue []

let lookupField ctx typeName fieldName =
    lookupType ctx typeName
    |> Option.bind (function | KlassLookupItem(_, _, table, _) -> table |> Lookup.tryFind fieldName)

let lookupVariable ctx name =
    ctx.VariableLookup
    |> Map.tryFind name

let insideLoopBody ctx =
    ctx.LoopDepth > 0

// context transformer
//

let restoreScope oldCtx newCtx =
    { newCtx with VariableLookup = oldCtx.VariableLookup }

let enterLoopBody ctx =
    { ctx with LoopDepth = ctx.LoopDepth + 1 }

let exitLoopBody ctx =
    { ctx with LoopDepth = ctx.LoopDepth - 1 }

let declareVariable name mut type' ctx =
    let var = VariableLookupItem(name, mut, type')
    { ctx with VariableLookup = ctx.VariableLookup |> Map.add name var }

let appendError msg ctx =
    { ctx with ErrorMessages = msg::ctx.ErrorMessages }

//
//

let makeEvalResult result ctx =
    Some result, ctx

let makeEvalError msg ctx =
    None, ctx |> appendError msg

let escapeEvalError ctx =
    None, ctx

let bindEvalResult f (x, ctx) =
    match x with
    | Some u ->
        f ctx u
    | None ->
        escapeEvalError ctx

let bindEvalResult2 f (x1, x2, ctx) =
    match x1, x2 with
    | Some u, Some v ->
        f ctx u v
    | _ ->
        escapeEvalError ctx

let bindEvalResult3 f (x1, x2, x3, ctx) =
    match x1, x2, x3 with
    | Some u, Some v, Some w ->
        f ctx u v w
    | _ ->
        escapeEvalError ctx

let processReturn result ctx =
    match result with
    | Ok y      -> makeEvalResult y ctx
    | Error msg -> makeEvalError msg ctx

let processEvalResult f (x, ctx) =
    match x with
    | Some u ->
        processReturn (f u) ctx
    | None ->
        escapeEvalError ctx

let processEvalResult2 f (x1, x2, ctx) =
    match x1, x2 with
    | Some u, Some v ->
        processReturn (f u v) ctx
    | _ ->
        escapeEvalError ctx

let processEvalResult3 f (x1, x2, x3, ctx) =
    match x1, x2, x3 with
    | Some u, Some v, Some w ->
        processReturn (f u v w) ctx
    | _ ->
        escapeEvalError ctx

let processEvalResultList f (xs, ctx) =
    if xs |> List.forall Option.isSome then
        processReturn (f (xs |> List.map Option.get)) ctx
    else
        escapeEvalError ctx

let updateContext f (x, ctx) =
    x, f ctx

let updateContext2 f (x1, x2, ctx) =
    x1, x2, f ctx

let updateContext3 f (x1, x2, x3, ctx) =
    x1, x2, x3, f ctx


type PendingMethod =
    | PendingMethod of MethodDefinition*SyntaxStmt

    member m.Definition =
        match m with PendingMethod(x, _) -> x
    member m.Body =
        match m with PendingMethod(_, x) -> x

type PendingKlass =
    | PendingKlass of KlassDefinition*PendingMethod list

    member m.Definition =
        match m with PendingKlass(x, _) -> x
    member m.MethodList =
        match m with PendingKlass(_, x) -> x

type TranslationSession(currentModule: BasicModuleInfo, importModules: BasicModuleInfo list) =
    let errorList = ResizeArray<ErrorMessage>()

    member m.CurrentModule =
        currentModule

    member m.HasError =
        errorList.Count > 0

    member m.AppendError msgList =
        errorList.AddRange(msgList)

    member m.LookupType name =
        Some <| SystemTypeIdent(Int)