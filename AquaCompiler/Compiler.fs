module Aqua.Compiler

open Aqua.Language
open Aqua.Syntax

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
    | EnumLookupItem of EnumDefinition
    | KlassLookupItem of KlassDefinition

type TranslationEnvironmentType =
    { TypeLookupTable: IDictionary<string, TypeLookupItem>;
      FunctionLookupTable: IDictionary<string*string, FunctionDefinition list> }

module TranslationEnvironment =

    // TODO: load imports
    let createEnvironment thisModule =
        let enums =
            thisModule.EnumList
            |> Seq.map (fun def -> def.Name, EnumLookupItem(def))
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
        match env.TypeLookupTable.TryGetValue name with
        | true, result  -> Some result
        | false, _      -> None

    let rec lookupFunction env typeName funcName =
        match env.FunctionLookupTable.TryGetValue ((typeName, funcName)) with
        | true, result                  -> result
        | false, _ when typeName <> ""  -> lookupFunction env "" funcName
        | false, _                      -> []

//
// Translation Context
//

type VariableLookupItem =
    | VariableLookupItem of string*MutablityModifier*TypeStub
with
    member m.Name =
        match m with | VariableLookupItem(name, _, _) -> name
    member m.Mutability =
        match m with | VariableLookupItem(_, mut, _) -> mut
    member m.Type =
        match m with | VariableLookupItem(_, _, type') -> type'

// NOTE as expression type cache always grows
// mutable collection is used for reference equality
type TranslationContextType =
    { CurrentFunction: FunctionDecl
      LoopDepth: int
      VariableLookup: Map<string, VariableLookupItem>
      LastExprType: TypeStub
      ExprTypeCache: Dictionary<Expression, TypeStub>
      ErrorMessages: ErrorMessageType list }
with
    member m.FunctionName =
        m.CurrentFunction.Name
    member m.ParamList =
        m.CurrentFunction.Declarator.ParamList
    member m.ReturnType =
        m.CurrentFunction.Declarator.ReturnType

module TranslationContext =
    let createContext func =
        { CurrentFunction = func
          LoopDepth = 0
          VariableLookup = func.Declarator.ParamList
                           |> List.map (fun (name, type') -> name, VariableLookupItem(name, Readonly, type'))
                           |> Map.ofList
          LastExprType = kUnitType
          ExprTypeCache = Dictionary(HashIdentity.Reference)
          ErrorMessages = [] }

    let withLastExprType ctx =
        ctx, ctx.LastExprType

    let restoreScope oldCtx newCtx =
        { newCtx with VariableLookup = oldCtx.VariableLookup }

    let enterLoopBody ctx =
        { ctx with LoopDepth = ctx.LoopDepth + 1 }

    let exitLoopBody ctx =
        { ctx with LoopDepth = ctx.LoopDepth - 1 }

    let declareVariable name mut type' ctx =
        let var = VariableLookupItem(name, mut, type')
        { ctx with VariableLookup = ctx.VariableLookup |> Map.add name var }    

    let cacheExprType expr type' ctx =
        ctx.ExprTypeCache.Add(expr, type')
        { ctx with LastExprType = type' }

    let appendError msg ctx =
        { ctx with ErrorMessages = msg::ctx.ErrorMessages }

//
// Translation Result
//
type TranslationResult =
    { ExprTypeLookup: IReadOnlyDictionary<Expression, TypeStub> }