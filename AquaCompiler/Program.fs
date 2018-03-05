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

[<EntryPoint>]
let main argv =
    let sampleCode = """
        module mycode

        import std
        fun foo(x: int) -> bool {
            var x = 1;
            val y = 2;
            x = y = 42;
            break

            if ((x==42) is aha) {
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
    
    printfn "%O" codePage
    Console.ReadKey() |> ignore

    0 // return an integer exit code
