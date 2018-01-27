module Aqua.Compiler

open Aqua.Language
open System.Collections.Generic

type TypeLookupItem =
    | EnumLookupItem of EnumDefinition
    | KlassLookupItem of KlassDefinition

type MethodLookupItem =
    | MethodLookupItem of KlassDefinition*KlassMethod

type FunctionLookupItem =
    | FunctionLookupItem of FunctionDefinition

type TranslationEnvironment =
    { TypeLookupTable: IDictionary<string, TypeLookupItem>;
      MethodLookupTable: IDictionary<string*string, MethodLookupItem>;
      FunctionLookupTable: IDictionary<string, FunctionLookupItem>; }