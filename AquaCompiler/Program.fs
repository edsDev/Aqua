open System
open Aqua.Language
open Aqua.Syntax
open Aqua.Parser
open Aqua.Compiler
open Aqua.Preprocessor
open Aqua.TypeChecker
open Aqua.Ast
open Aqua.CodeGenerator
open Aqua.Bytecode

//
// Reference Lookup Path: string list
type ProjectSession = class end

// first pass: parse the source file
// second pass: enumerate and memorize declarations
// third pass: do type checking

[<EntryPoint>]
let main argv =
    let sampleCode = """
        module mycode

        //import std
        class Program {
            public static fun foo(x: int) -> bool {
                val y = 2;

                if ((x==42) is bool) {
                    return true;
                }
                else {
                    return false;
                }

                return y is int;
            }

            // line comment
            public static fun sum(x: int, y: int) -> int {
                return x + /* block comment */ y;
            }
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

    // module loader
    let loader = ModuleLoader([])

    // parse
    let codePage =
        match parseCodePage sampleCode with
        | Ok t -> t
        | Error e -> printfn "%s" e; failwith e

    // preprocessing
    let session, pendingKlassList =
        preprocessModule loader codePage

    // type checking
    let astKlassList =
        translateModule session pendingKlassList

    // code generation
    let ss = Seq.toList <| seq {
        for AstKlass(klassDef, methodList) in astKlassList do
            for AstMethod(methodDef, body) in methodList do
                let codeAcc = CodeGen.createEmpty ()
                
                compileStmt () () codeAcc body
                yield methodDef.Name, codeAcc.ToArray()
    }

    printfn "%A" ss
    Console.ReadKey() |> ignore

    0 // return an integer exit code
