module Aqua.Compiler

open Aqua.LookupUtils
open Aqua.Language
open Aqua.Syntax
open Aqua.Ast

type ErrorMessage =
    { ReferenceRange: Range; Message: string; }

type ErrorMessageList = ErrorMessage list

//
// Compiler Session
//
type CompilerSession() = class
    //let mutable private _moduleCache = Dictionary<ModuleIdent, BasicModuleInfo>()
    end

//
// Translation Environment
//
type TypeLookupItem =
    //| EnumLookupItem of EnumDefinition
    | KlassLookupItem of ModuleIdent*KlassDefinition

type FunctionLookupItem =
    | FunctionLookupItem of ModuleIdent*FunctionDefinition

// <type name> -> <definition>
type TypeLookupTable = Lookup<string, TypeLookupItem>

// <type name>, <function name> -> <definition list>
// NOTE type name of global function should be empty
type FunctionLookupTable = Lookup<string*string, FunctionDefinition list>

type TranslationEnvironment =
    { CurrentInstance: KlassDefinition option
      CurrentFunction: FunctionDefinition
      TypeLookup: TypeLookupTable
      FunctionLookup: FunctionLookupTable }

type VariableLookupItem =
    | VariableLookupItem of string*MutabilityModifier*TypeStub

    member m.Name =
        match m with | VariableLookupItem(name, _, _) -> name
    member m.Mutability =
        match m with | VariableLookupItem(_, mut, _) -> mut
    member m.Type =
        match m with | VariableLookupItem(_, _, type') -> type'

// NOTE as expression type cache always grows
// mutable collection is used for reference equality
type TranslationContext =
    { Environment: TranslationEnvironment

      LoopDepth: int
      VariableLookup: Map<string, VariableLookupItem>
      ErrorMessages: ErrorMessageList }

let initContext env =
    let varLookup = Map.empty

    { Environment = env
      LoopDepth = 0
      VariableLookup = varLookup
      ErrorMessages = [] }

// context proxy
//
let getEnvironment ctx =
    ctx.Environment

let getCurrentInstance ctx =
    ctx.Environment.CurrentInstance

let getCurrentFunction ctx =
    ctx.Environment.CurrentFunction

let lookupType ctx name =
     ctx.Environment.TypeLookup
     |> Lookup.tryFind name

let lookupFunction ctx funcName = 
    ctx.Environment.FunctionLookup
    |> Lookup.tryFind ("", funcName)
    |> Option.defaultValue []

let lookupMethod ctx typeName funcName =
    ctx.Environment.FunctionLookup
    |> Lookup.tryFind (typeName, funcName)
    |> Option.defaultValue []

let lookupField ctx typeName fieldName =
    lookupType ctx typeName
    |> Option.bind 
           (function
            | KlassLookupItem(_, def) -> def.Fields |> Lookup.tryFind fieldName)

let isStaticContext ctx =
    ctx |> getCurrentInstance |> Option.isNone

let isInstanceContext ctx =
    ctx |> getCurrentInstance |> Option.isSome

let getInstanceName ctx =
    ctx
    |> getCurrentInstance
    |> Option.map (fun x -> x.Name)
    |> Option.defaultValue ""

let getFunctionName ctx =
    (getCurrentFunction ctx).Name

let getReturnType ctx =
    (getCurrentFunction ctx).Signature.ReturnType

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
        
type FunctionRecord =
    | FunctionRecord of FunctionDefinition*FunctionDecl

type MethodRecord =
    | MethodRecord of KlassDefinition*FunctionDefinition*FunctionDecl
type KlassRecord =
    | KlassRecord of KlassDefinition*MethodRecord list

type TranslationMaterial =
    { ModuleName: ModuleIdent
      ImportList: ModuleIdent list
      FunctionRecords: FunctionRecord list
      KlassRecords: KlassRecord list }