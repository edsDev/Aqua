open System
open Aqua.Language
open Aqua.Syntax
open Aqua.Parser
open Aqua.Compiler
open Aqua.Preprocessor
open Aqua.TypeChecker

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

    // parse
    let codePage =
        match parseCodePage sampleCode with
        | Success t -> t
        | Failure e -> printfn "%s" e; failwith e

    // preprocess
    let session, pendingKlassList =
        let loader = ModuleLoader([])
        preprocessModule loader codePage

    let astKlassList =
        translateModule session pendingKlassList

    printfn "%O" astKlassList
    Console.ReadKey() |> ignore

    0 // return an integer exit code
