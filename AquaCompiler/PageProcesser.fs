module Aqua.PageProcessor

open Aqua.ResultUtils
open Aqua.CollectionUtils
open Aqua.Language
open Aqua.Syntax
open Aqua.ErrorMessage
open Aqua.Compiler
open Aqua.Ast

// helpers
//

let rec private translateTypeListLite lookupProxy typeList =
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

let private processField lookupProxy (decl: FieldDecl) =
    translateTypeLite lookupProxy decl.Type
    |> Result.map (fun t -> FieldDefinition(decl.Name, decl.Modifiers, decl.Mutability, t))

let private processMethod lookupProxy (decl: MethodDecl) =
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

let private processKlass lookupType (decl: KlassDecl) =
    let fields, fieldErrors =
        decl.Fields
        |> List.map (processField lookupType)
        |> Result.rearrange

    let methods, methodErrors =
        decl.Methods
        |> List.map (processMethod lookupType)
        |> Result.rearrange

    if [fieldErrors; methodErrors] |> List.forall List.isEmpty then
        let fieldDefList = fields
        let methodDefList = methods |> List.map (fun x -> x.Definition)

        Ok <| PendingKlass(KlassDefinition(decl.Name, decl.Modifiers, fieldDefList, methodDefList), methods)
    else
        Error <| ((fieldErrors @ methodErrors) |> List.collect id)

let private createTypeLookupItem moduleIdent klassDef =
    let fieldLookup =
        KlassDefinition.getFields klassDef
        |> Seq.map (fun field -> FieldDefinition.getName field, field)
        |> DictView.ofSeq

    let methodLookup =
        KlassDefinition.getMethods klassDef
        |> Seq.groupBy MethodDefinition.getName
        |> Seq.map (fun (name, methods) -> name, List.ofSeq methods)
        |> DictView.ofSeq

    let klassRef = KlassReference(moduleIdent, klassDef)
    KlassLookupItem(klassRef, fieldLookup, methodLookup)

let processModule (loader: ModuleLoader) decl =  
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
                let publicKlassList = 
                    moduleInfo.KlassList 
                    |> Seq.filter (KlassDefinition.getModifiers >> ModifierGroup.isPublic)

                for klass in publicKlassList do
                    let klassName = 
                        KlassDefinition.getName klass

                    yield klassName, UserTypeIdent(moduleInfo.ModuleName, klassName)
        }

    let lookupProxy name =
        typeIdentLookup |> DictView.tryFind name

    let pendingKlassList =
        Seq.toList <| seq {
            for klass in decl.KlassList do
                match processKlass lookupProxy klass with
                | Ok record ->
                    yield record
                | Error msgs ->
                    errorBuffer.AddRange(msgs)
        }

    let currentModule =
        { ModuleName = currentModuleName
          ImportList = importedModules |> List.map (fun x -> x.ModuleName)
          KlassList = pendingKlassList |> List.map (fun x -> x.Definition) }

    let typeLookup =
        DictView.ofSeq <| seq {
            for PendingKlass(def, _) in pendingKlassList do
                let klassType = UserTypeIdent(currentModuleName, def.Name)
                yield (getTypeName klassType), (createTypeLookupItem currentModuleName def)

        }

    if errorBuffer.Count = 0 then
        Ok <| { CurrentModule = currentModule
                
                TypeLookup = typeLookup
                PendingKlassList = pendingKlassList }
    else
        Error <| TranslationError errorBuffer
