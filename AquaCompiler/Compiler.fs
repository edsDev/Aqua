module Aqua.Compiler

open Aqua.CollectionUtils
open Aqua.Language
open Aqua.Syntax
open Aqua.Ast
open System.IO
open System.Collections.Generic
open FSharpx.Collections

type ErrorMessage =
    { ReferenceRange: SynRange; Message: string; }

type ErrorMessageList = ErrorMessage list

type CompilationError =
    | ParsingError of string
    | TranslationError of ArrayView<ErrorMessage>

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
type FieldLookupTable = DictView<string, FieldDefinition>
// <function name> -> <definition list>
type MethodLookupTable = DictView<string, MethodDefinition list>

type KlassAccessRecord =
    | KlassLookupItem of KlassReference*FieldLookupTable*MethodLookupTable

    member m.Klass =
        m |> function KlassLookupItem(x, _, _) -> x
    member m.FieldLookup =
        m |> function KlassLookupItem(_, x, _) -> x
    member m.MethodLookup =
        m |> function KlassLookupItem(_, _, x) -> x

    member m.Type =
        m.Klass.Type

// <type name> -> <definition>
type TypeLookupTable = DictView<string, KlassAccessRecord>

type NameAccessRecord =
    | ArgumentLookupItem of int*string*TypeIdent
    | VariableLookupItem of int*string*TypeIdent*MutabilitySpec

type TranslationEnvironment =
    { CurrentModule: ModuleIdent
      CurrentKlass: KlassDefinition
      CurrentMethod: MethodDefinition

      TypeLookup: TypeLookupTable }

type TranslationContext =
    { Environment: TranslationEnvironment

      LoopDepth: int
      VariableList: PersistentVector<AstVariableDecl>
      VariableLookup: Map<string, NameAccessRecord>
      ErrorMessages: ErrorMessageList }

let createContext env =
    let varLookup =
        let offset = 
            env.CurrentMethod
            |> MethodDefinition.getModifiers
            |> ModifierGroup.getLifetimeType
            |> function | LifetimeModifier.Static   -> 0 
                        | LifetimeModifier.Instance -> 1

        env.CurrentMethod.Parameters
        |> Seq.mapi (fun i (name, t) -> name, ArgumentLookupItem(i + offset, name, t))
        |> Map.ofSeq

    { Environment = env

      LoopDepth = 0
      VariableList = PersistentVector.empty
      VariableLookup = varLookup
      ErrorMessages = [] }

module TranslationContext =
    // element proxy
    //

    let getCurrentKlass ctx =
        ctx.Environment.CurrentKlass

    let getCurrentMethod ctx =
        ctx.Environment.CurrentMethod

    let getCurrentKlassType ctx =
        let env = ctx.Environment
        UserTypeIdent(env.CurrentModule, env.CurrentKlass.Name)

    let getMethodName ctx =
        ctx |> getCurrentMethod |> MethodDefinition.getName

    let getReturnType ctx =
        ctx |> getCurrentMethod |> MethodDefinition.getReturnType

    let isInstanceContext ctx =
        getCurrentMethod ctx
        |> MethodDefinition.getModifiers 
        |> ModifierGroup.isInstance

    let getNextVarId ctx =
        PersistentVector.length ctx.VariableList

    let private lookupTypeRecord ctx name =
         ctx.Environment.TypeLookup
         |> DictView.tryFind name

    let lookupKlass ctx name =
        lookupTypeRecord ctx name
        |> Option.map (fun klass -> klass.Klass)

    let lookupMethod ctx typeName funcName =
        match lookupTypeRecord ctx typeName with
        | Some type' ->
            type'.MethodLookup
            |> DictView.tryFind funcName
            |> Option.bind (fun funcList -> Some (type'.Klass, funcList))

        | None ->
            None

    let lookupField ctx typeName fieldName =
        match lookupTypeRecord ctx typeName with
        | Some type' ->
            type'.FieldLookup 
            |> DictView.tryFind fieldName
            |> Option.map (fun field -> type'.Klass, field)

        | None ->
            None
        
    let lookupVariable ctx name =
        ctx.VariableLookup
        |> Map.tryFind name

    let lookupVariableById ctx id =
        ctx.VariableList
        |> PersistentVector.nth id

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
        let id = PersistentVector.length ctx.VariableList
        let decl = AstVariableDecl(name, type', mut)
        let record = VariableLookupItem(id, name, type', mut)

        { ctx with VariableList = ctx.VariableList |> PersistentVector.conj decl
                   VariableLookup = ctx.VariableLookup |> Map.add name record }

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
        m |> function PendingMethod(x, _) -> x
    member m.Body =
        m |> function PendingMethod(_, x) -> x

type PendingKlass =
    | PendingKlass of KlassDefinition*PendingMethod list

    member m.Definition =
        m |> function PendingKlass(x, _) -> x
    member m.MethodList =
        m |> function PendingKlass(_, x) -> x

type TranslationSession = 
    { CurrentModule: BasicModuleInfo
      
      TypeLookup: DictView<string, KlassAccessRecord>
      PendingKlassList: PendingKlass list }