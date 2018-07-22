module Aqua.Module

open Aqua.Language

type BasicModuleInfo =
    { ModuleName: ModuleIdent
      ImportList: ModuleIdent list
      KlassList: KlassDefinition list }

let loadModule env name =
    ()

let storeModule path info codes =
    ()