module Aqua.Compiler

open Aqua.LookupUtils
open Aqua.Language
open Aqua.Syntax
open Aqua.Ast

open System.Collections.Generic

type ErrorMessageType =
    { ReferenceRange: Syntax.Range; Message: string; }

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
    | KlassLookupItem of KlassDefinition

type FunctionLookupItem =
    | FunctionLookupItem of ModuleIdent*FunctionDefinition

type TranslationEnvironment =
    { // type name -> definition
      TypeLookupTable: Lookup<string, TypeLookupItem>;
      // type name, function name -> definition list
      // NOTE type name of global function should be empty
      FunctionLookupTable: Lookup<string*string, FunctionDefinition list> }

module TranslationEnvironment =

    // TODO: load imports
    let createEnvironment thisModule =
        let enums = []
        //    thisModule.EnumList
        //    |> Seq.map (fun def -> def.Name, EnumLookupItem(def))
        let klasses = 
            thisModule.KlassList 
            |> Seq.map (fun def -> def.Name, KlassLookupItem(def))

        let methods = []
        let functions =
            thisModule.FunctionList
            |> Seq.groupBy (fun def -> def.Name)
            |> Seq.map (fun (name, fs) -> ("", name), List.ofSeq fs) 

        { TypeLookupTable = dict <| Seq.append enums klasses;
          FunctionLookupTable = dict <| Seq.append methods functions; }

    let lookupType env name =
        env.TypeLookupTable |> Lookup.tryFind name

    let lookupFunction env funcName =
        env.FunctionLookupTable |> Lookup.tryFind ("", funcName)

    let lookupMethod env typeName funcName =
        env.FunctionLookupTable |> Lookup.tryFind (typeName, funcName)

    let lookupCallableName env typeName funcName =
        match lookupMethod env typeName funcName with
        | Some x                    -> x
        | None when typeName<>""    -> lookupFunction env funcName |> Option.defaultValue []
        | None                      -> []

    let lookupField env typeName fieldName =
        lookupType env typeName
        |> Option.bind (function
                        | KlassLookupItem(def) -> def.Fields |> Lookup.tryFind fieldName )
//
// Translation Context
//

type VariableLookupItem =
    | VariableLookupItem of string*MutablityModifier*TypeStub

    member m.Name =
        match m with | VariableLookupItem(name, _, _) -> name
    member m.Mutability =
        match m with | VariableLookupItem(_, mut, _) -> mut
    member m.Type =
        match m with | VariableLookupItem(_, _, type') -> type'

// NOTE as expression type cache always grows
// mutable collection is used for reference equality
type TranslationContext =
    { CurrentFunction: FunctionDecl
      LoopDepth: int
      VariableLookup: Map<string, VariableLookupItem>
      ErrorMessages: ErrorMessageType list }

    member m.FunctionName =
        m.CurrentFunction.Name
    member m.ParamList =
        m.CurrentFunction.Declarator.ParamList
    member m.ReturnType =
        m.CurrentFunction.Declarator.ReturnType

module TranslationContext =

    // context builder
    let createContext func =
        { CurrentFunction = func
          LoopDepth = 0
          VariableLookup = func.Declarator.ParamList
                           |> List.map (fun (name, type') -> name, VariableLookupItem(name, Readonly, type'.Stub))
                           |> Map.ofList

          ErrorMessages = [] }

    // context proxy
    let lookupVariable ctx name =
        ctx.VariableLookup |> Map.tryFind name

    // context transformer
    let withIdentity ctx =
        ctx

    let restoreScope oldCtx newCtx =
        { newCtx with VariableLookup = oldCtx.VariableLookup }

    let testLoopBody ctx =
        ctx.LoopDepth > 0

    let enterLoopBody ctx =
        { ctx with LoopDepth = ctx.LoopDepth + 1 }

    let exitLoopBody ctx =
        { ctx with LoopDepth = ctx.LoopDepth - 1 }

    let declareVariable name mut type' ctx =
        let var = VariableLookupItem(name, mut, type')
        { ctx with VariableLookup = ctx.VariableLookup |> Map.add name var }    

    //let registerExprType type' expr ctx =
    //    ctx.ExprTypeCache.Add(expr, type')
    //    { ctx with LastExprType = type' }

    let appendError msg ctx =
        { ctx with ErrorMessages = msg::ctx.ErrorMessages }

    let makeEvalResult result ctx =
        Some result, ctx

    let makeEvalErrorEscape ctx =
        None, ctx

    let makeEvalError msg ctx =
        None, ctx |> appendError msg

    let bindEvalResult f (x, ctx) =
        match x with
        | Some u ->
            match f u with
            | Ok y -> makeEvalResult y ctx
            | Error msg -> makeEvalError msg ctx
        | None ->
            makeEvalErrorEscape ctx

    let bindEvalResult2 f (x1, x2, ctx) =
        match x1, x2 with
        | Some u, Some v ->
            match f u v with
            | Ok y -> makeEvalResult y ctx
            | Error msg -> makeEvalError msg ctx
        | _ ->
            makeEvalErrorEscape ctx

    let bindEvalResult3 f (x1, x2, x3, ctx) =
        match x1, x2, x3 with
        | Some u, Some v, Some w ->
            match f u v w with
            | Ok y -> makeEvalResult y ctx
            | Error msg -> makeEvalError msg ctx
        | _ ->
            makeEvalErrorEscape ctx

    let bindEvalResultList f (xs, ctx) =
        if xs |> List.forall Option.isSome then
            match f (xs |> List.map Option.get) with
            | Ok y -> makeEvalResult y ctx
            | Error msg -> makeEvalError msg ctx
        else
            makeEvalErrorEscape ctx

    let updateContext f (x, ctx) =
        x, f ctx

    let updateContext2 f (x1, x2, ctx) =
        x1, x2, f ctx

    let updateContext3 f (x1, x2, x3, ctx) =
        x1, x2, x3, f ctx
        
