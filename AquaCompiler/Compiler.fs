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

type TranslationInfo =
    { CurrentModule: ModuleIdent
      CurrentKlass: KlassDefinition
      CurrentMethod: MethodDefinition

      TypeLookup: TypeLookupTable }

type TranslationContext =
    { GlobalInfo: TranslationInfo

      LoopDepth: int
      VariableList: PersistentVector<AstVariableDecl>
      VariableLookup: Map<string, NameAccessRecord>
      ErrorMessages: ErrorMessageList }

type TranslationEnv = TranslationContext*SynRange

let createContext info =
    let varLookup =
        let offset = 
            info.CurrentMethod
            |> MethodDefinition.getModifiers
            |> ModifierGroup.getLifetimeType
            |> function | LifetimeModifier.Static   -> 0 
                        | LifetimeModifier.Instance -> 1

        info.CurrentMethod.Parameters
        |> Seq.mapi (fun i (name, t) -> name, ArgumentLookupItem(i + offset, name, t))
        |> Map.ofSeq

    { GlobalInfo = info

      LoopDepth = 0
      VariableList = PersistentVector.empty
      VariableLookup = varLookup
      ErrorMessages = [] }

module TranslationContext =
    // element proxy
    //
    let getCurrentKlass ctx =
        ctx.GlobalInfo.CurrentKlass

    let getCurrentMethod ctx =
        ctx.GlobalInfo.CurrentMethod

    let getCurrentKlassType ctx =
        let env = ctx.GlobalInfo
        UserTypeIdent(env.CurrentModule, env.CurrentKlass.Name)

    let getCurrentMethodName ctx =
        ctx |> getCurrentMethod |> MethodDefinition.getName

    let getCurrentReturnType ctx =
        ctx |> getCurrentMethod |> MethodDefinition.getReturnType

    let isInstanceContext ctx =
        getCurrentMethod ctx
        |> MethodDefinition.getModifiers 
        |> ModifierGroup.isInstance

    let getNextVarId ctx =
        PersistentVector.length ctx.VariableList

    let private lookupTypeRecord ctx name =
         ctx.GlobalInfo.TypeLookup
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

    // context
    //
    let getContext =
        fun (ctx: TranslationContext) -> ctx, ctx

    let restoreScope oldCtx ctx =
        (), { ctx with VariableLookup = oldCtx.VariableLookup }

    let enterLoopBody ctx =
        (), { ctx with LoopDepth = ctx.LoopDepth + 1 }

    let exitLoopBody ctx =
        (), { ctx with LoopDepth = ctx.LoopDepth - 1 }

    let declareVariable name mut type' ctx =
        let id = PersistentVector.length ctx.VariableList
        let decl = AstVariableDecl(name, type', mut)
        let record = VariableLookupItem(id, name, type', mut)

        (), { ctx with VariableList = ctx.VariableList |> PersistentVector.conj decl
                       VariableLookup = ctx.VariableLookup |> Map.add name record }

    let appendError msg ctx =
        (), { ctx with ErrorMessages = msg::ctx.ErrorMessages }

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


module Translation =
    let bind k m = 
        fun (ctx: TranslationContext) -> let (a, ctx') = m ctx in (k a) ctx'

    let return' a = 
        fun (ctx: TranslationContext) -> a, ctx

    let returnFrom (m: _ * TranslationContext) = 
        m

    let tryWith m h =
        fun (ctx: TranslationContext) -> try m ctx with e -> h e ctx

    let tryFinally m h =
        fun (ctx: TranslationContext) -> try m ctx finally h()
        
    let yield' zero error =
        fun (ctx: TranslationContext) -> zero, { ctx with ErrorMessages = error::ctx.ErrorMessages}

    type TranslationBuilder<'a>(zero: 'a) =
        member __.Bind(m, k) = bind k m
        member __.Return(a) = return' a
        member __.ReturnFrom(m) = returnFrom m
        member __.Zero() = return' zero
        member __.TryWith(m, h) = tryWith m h
        member __.TryFinally(m, h) = tryFinally m h
        member __.Delay(f) = bind f (return' zero)
        member __.Yield(error) = yield' zero error

let translateTypeM = Translation.TranslationBuilder(DummyTypeIdent)
let translateExprM = Translation.TranslationBuilder(Ast_DummyExpr)
let translateStmtM = Translation.TranslationBuilder(Ast_DummyStmt)
