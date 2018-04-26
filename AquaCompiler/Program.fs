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
                var z = 42;

                if ((x==42) is bool) {
                    return y==z;
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

    sampleCode
    |> parseModule
    |> Result.bind (preprocessModule loader)
    |> Result.bind translateModule
    |> function
       | Ok astKlassList ->
            let ss = Seq.toList <| seq {
                for AstKlass(klassDef, methodList) in astKlassList do
                    for AstMethod(methodDef, varList, body) in methodList do
                        let codeAcc = CodeGen.createEmpty ()
                
                        compileStmt () () codeAcc body
                        yield methodDef.Name, codeAcc.ToArray()
            }
            printfn "%A" ss
       | Error (ParsingError e) ->
            printfn "%A" e
       | Error (TranslationError e) ->
            printfn "%A" e


    Console.ReadKey() |> ignore

    0 // return an integer exit code
