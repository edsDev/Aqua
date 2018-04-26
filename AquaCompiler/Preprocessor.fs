module Aqua.Preprocessor

open Aqua.ResultUtils
open Aqua.CollectionUtils
open Aqua.Language
open Aqua.Syntax
open Aqua.Ast
open Aqua.ErrorMessage
open Aqua.Compiler
open System.Linq

// helpers
//

let rec translateTypeListLite lookupProxy typeList =
    let translateTypeAux type' =
        match type' with
        | Syn_SystemType(_, category) ->
            Ok <| SystemTypeIdent category

        | Syn_UserType(rg, name) ->
            match lookupProxy name with
            | Some x -> Ok <| x
            | None -> Error <| [invalidUserType rg name]

        | Syn_FunctionType(_, paramTypeList, retType) ->
            retType::paramTypeList
            |> translateTypeListLite lookupProxy
            |> Result.map (fun ts ->
                               let paramsStub = ts |> List.tail
                               let returnStub = ts |> List.head

                               makeFunctionTypeIdent paramsStub returnStub)

    let resultList = List.map translateTypeAux typeList
    let isOk = function | Ok _ -> true | Error _ -> false

    if resultList |> List.forall isOk then
        resultList
        |> List.choose (function | Ok x -> Some x | Error _ -> None)
        |> Ok
    else
        resultList
        |> List.map (function | Ok _ -> [] | Error msgList -> msgList)
        |> List.reduce List.append
        |> Error

let translateTypeLite lookupProxy type' =
    translateTypeListLite lookupProxy [type']
    |> Result.map List.exactlyOne

// preprocessing
//

let preprocessField lookupProxy (decl: FieldDecl) =
    Error []

let preprocessMethod lookupProxy (decl: MethodDecl) =

    let paramNameList, paramTypeList =
        List.unzip decl.Declarator.ParamList

    let retType =
        decl.Declarator.ReturnType

    retType::paramTypeList
    |> translateTypeListLite lookupProxy
    |> Result.map (fun ts ->
                       let translatedParamTypes = ts |> List.tail
                       let translatedReturnType = ts |> List.head

                       let paramList = List.zip paramNameList translatedParamTypes
                       let definition = MethodDefinition(decl.Name, decl.Modifiers, paramList, translatedReturnType)

                       PendingMethod(definition, decl.Body))

let preprocessKlass lookupType (decl: KlassDecl) =
    let fields, fieldErrors =
        decl.Fields
        |> List.map (preprocessField lookupType)
        |> Result.rearrange

    let methods, methodErrors =
        decl.Methods
        |> List.map (preprocessMethod lookupType)
        |> Result.rearrange

    if fieldErrors |> List.isEmpty && methodErrors |> List.isEmpty then
        let fieldDefList = fields
        let methodDefList = methods |> List.map (fun x -> x.Definition)

        Ok <| PendingKlass(KlassDefinition(decl.Name, fieldDefList, methodDefList), methods)
    else
        Error <| List.append (fieldErrors |> List.collect id) (methodErrors |> List.collect id)

let preprocessModule (loader: ModuleLoader) decl =
    // TODO: remove mutable buffer
    let errorBuffer = ResizeArray()

    let currentModuleName =
        decl.ModuleInfo.Identifier

    let importedModules =
        List.ofSeq <| seq {
            for m in decl.ImportList do
                match loader.LoadModule m.Identifier with
                | Some info -> yield info
                | None -> failwith ""
        }

    let typeIdentLookup =
        DictView.ofSeq <| seq {
            // internal types
            for klassInfo in decl.KlassList do
                yield klassInfo.Name, UserTypeIdent(currentModuleName, klassInfo.Name)

            // external types
            for moduleInfo in importedModules do
                for klassInfo in moduleInfo.KlassList do
                    yield klassInfo.Name, UserTypeIdent(moduleInfo.ModuleName, klassInfo.Name)
        }

    let lookupProxy name =
        typeIdentLookup |> DictView.tryFind name

    let pendingKlassList =
        Seq.toList <| seq {
            for klass in decl.KlassList do
                match preprocessKlass lookupProxy klass with
                | Ok record ->
                    yield record
                | Error msgs ->
                    errorBuffer.AddRange(msgs)
        }

    let currentModule =
        { ModuleName = currentModuleName
          ImportList = importedModules |> List.map (fun x -> x.ModuleName)
          KlassList = pendingKlassList |> List.map (fun x -> x.Definition) }


    if errorBuffer.Count = 0 then
        Ok <| { CurrentModule = currentModule
                ImportedModules = importedModules
                
                PendingKlassList = pendingKlassList }
    else
        Error <| TranslationError errorBuffer
