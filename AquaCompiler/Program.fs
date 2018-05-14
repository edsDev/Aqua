open System
open Aqua.Language
open Aqua.Syntax
open Aqua.Parser
open Aqua.Compiler
open Aqua.PageProcessor
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

        class Point {
            public fun SetX(t: int) -> unit {
                this.x = t;
            }
            public fun SetY(t: int) -> unit {
                this.y = t;
            }

            public fun GetX() -> int {
                return this.x;
            }
            public fun GetY() -> int {
                return this.y;
            }

            public fun DistToOrigin() -> int {
                val x = this.GetX();
                val y = this.GetY();

                return x*x + y*y;
            }

            var x: int;
            var y: int;
        }
    """

    // module loader
    let loader = ModuleLoader([])

    sampleCode
    |> parseModule
    |> Result.bind (processModule loader)
    |> Result.bind translateModule
    |> function
       | Ok astKlassList ->
            Seq.toList <| seq {
                for AstKlass(klass, methodList) in astKlassList do
                    for AstMethod(method, varList, body) in methodList do
                        let codeAcc = CodeGen.createEmpty ()
                
                        compileStmt () codeAcc body
                        yield MethodDefinition.getName method, codeAcc.ToArray()
            }
            |> List.iter (printfn "%A")
       | Error (ParsingError e) ->
            printfn "%s" e
       | Error (TranslationError e) ->
            printfn "%A" (e |> Seq.toList)


    Console.ReadKey() |> ignore

    0 // return an integer exit code
