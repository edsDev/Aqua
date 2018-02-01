open System
open Aqua.Language
open Aqua.Syntax
open Aqua.Parser
open Aqua.Compiler
open Aqua

// 
// Reference Lookup Path: string list
type ProjectSession = class end

// first pass: parse the source file
// second pass: enumerate and memorize declarations
// third pass: do type checking

let translateModuleDecl (ModuleDecl(name)) = name

let translateImportDecl (ImportDecl(name)) = name

let translateKlassDecl (KlassDecl(name))= KlassDefinition(name, [], [])

let translateFunctionDecl = function
    | FunctionDecl(name, FunctionDeclarator(paramDecl, retType), _) -> 
        let paramTypes = 
            paramDecl |> List.map snd
        
        FunctionDefinition(name, Public, FunctionSignature(paramTypes, retType))

// TODO: add enum support
let extractModuleInfo syntaxTree =
    { ModuleName    = syntaxTree.ModuleInfo |> translateModuleDecl;
      ImportList    = syntaxTree.Imports    |> List.map translateImportDecl;
      EnumList      = [];
      KlassList     = syntaxTree.Klasses    |> List.map translateKlassDecl;
      FunctionList  = syntaxTree.Functions  |> List.map translateFunctionDecl; }

[<EntryPoint>]
let main argv =
    let sampleCode = """
        module mycode

        import std
        fun foo(x: int) -> bool {
            break

            if ((x==42) is bool) {
                return true;
            }
            else {
                return false;
            }

            return 42
        }

        // line comment
        fun sum(x: int, y: int) -> bool {
            return x + /* block comment */ y;
        }

        fun apply(f: (int, int) -> int, x: int, y: int) -> int {
            return f(x, y) + sum(x, y) + g(x, y) + z;
        }

        /*
        class Point {

            constructor(a: int, b: int) {
                x = a;
                y = b;
            }

            fun DistToOrigin() -> int {
                return x*x + y*y;
            }

            val x: int;
            val y: int;
        }
        */
    """
   
    // parse
    let codePage = 
        match parseCodePage sampleCode with
        | Success t -> t
        | Failure e -> printfn "%s" e; failwith e

    let moduleInfo = extractModuleInfo codePage
    let env = TranslationEnvironment.createEnvironment moduleInfo

    let translateError { ReferenceRange=rg; Message=msg } =
        let srcCode = sampleCode.Substring(rg.StartIndex, rg.Length).Trim()
        sprintf "[Line:%d;Column:%d]\n\"%s\"\n%s" rg.StartLine rg.StartColumn srcCode msg
    
    let mergeError =
        List.map translateError
        >> String.concat "\n"

    let xxs =
        codePage.Functions
        |> List.map (TypeChecker.checkFunction env)
        |> List.map (fun ctx -> sprintf "in function %s:\n" ctx.FunctionName + mergeError ctx.ErrorMessages)
        |> String.concat "\n\n"
    
    printfn "%s" xxs
    Console.ReadKey() |> ignore

    0 // return an integer exit code
