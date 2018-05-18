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

        // NAME LOOKUP: METHOD
        // [IMPLICIT LOOKUP]
        // in static method, search for static method only
        // in instance method, search for both static and instance method
        //
        // [EXPLICIT LOOKUP]
        // <expr>.<method>(...) looks for instance method
        // <type>.<method>(...) looks for static method

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

            public static fun SumTo(n: int) -> int {

                if (n<1) return 0;

                var result = 0;
                var x = 0;

                while (x<n) {
                    x = x + 1;
                    result = result + x;

                    if (x==41)
                        continue;
                }
                
                return result;
            }
        }

        class Foo {
            private fun Bar() -> unit {}
        }

        class Point {
            public fun DistToOrigin() -> int {
                val x = GetX();
                val y = this.GetY();

                return x*x + y*y;
            }

            public static fun Test(foo: Foo) -> unit {
                // this.DistToOrigin();
                // foo.Bar();
            }

            public fun SetX(t: int) -> unit {
                x = t;
            }
            public fun SetY(t: int) -> unit {
                y = t;
            }

            public fun GetX() -> int {
                return x;
            }
            public fun GetY() -> int {
                return y;
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
                
                        compileStmt { LoopStart = 0 } codeAcc body |> ignore
                        yield method.Name, codeAcc.ToArray()
            }
            |> List.iter (printfn "%A")
       | Error (ParsingError e) ->
            printfn "%s" e
       | Error (TranslationError e) ->
            printfn "%A" (e |> Seq.toList)


    Console.ReadKey() |> ignore

    0 // return an integer exit code
