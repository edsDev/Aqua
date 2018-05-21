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
open System.Linq.Expressions

// first pass: parse the source file
// second pass: enumerate and memorize declarations
// third pass: do type checking

let modifierToString ms =
    [ (if ModifierGroup.isStatic ms then "static" else "instance")
      (if ModifierGroup.isPublic ms then "public" else "private") ]
    |> List.filter (not << String.IsNullOrEmpty)
    |> fun xs -> String.Join(", ", xs)

let fieldRefToString (field: FieldReference) =
    sprintf "%s::%s.%s" 
            (field.Module.ToString()) 
            (field.Klass.Name) 
            (field.Definition.Name)

let methodRefToString (method: MethodReference) =
    let paramListStr =
        method.Definition.Parameters
        |> List.map (snd >> TypeIdent.getTypeName)
        |> fun xs -> String.Join(", ", xs)

    sprintf "%s::%s.%s(%s)" 
            (method.Module.ToString()) 
            (method.Klass.Name) 
            (method.Definition.Name)
            (paramListStr)

let bytecodeToString code =
    match code with
    | LoadField(field) ->
        sprintf "LoadField \"%s\"" (fieldRefToString field)
    | StoreField(field) ->
        sprintf "StoreField \"%s\"" (fieldRefToString field)
    | CastObj(t) ->
        sprintf "CastObj \"%s\"" (t.ToString())
    | NewObj(ctor) ->
        sprintf "NewObj \"%s\"" (methodRefToString ctor)
    | Call(method) ->
        sprintf "Call \"%s\"" (methodRefToString method)
    | _ -> 
        sprintf "%A" code

[<EntryPoint>]
let main argv =
    let sampleCode = """
        module Example

        // NAME LOOKUP: METHOD
        // [IMPLICIT LOOKUP]
        // in static method, search for static method only
        // in instance method, search for both static and instance method
        //
        // [EXPLICIT LOOKUP]
        // <expr>.<method>(...) looks for instance method
        // <type>.<method>(...) looks for static method

        //import System
        static class Program {
            public static fun Foo(x: int) -> bool {
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
            public static fun Sum(x: int, y: int) -> int {
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
                        break;
                }
                
                return result;
            }

            public static fun Main() -> unit {
                var p1 = new Point(1, 2);
            }
        }

        class Foo {
            private fun Bar() -> unit {}
        }

        class Point {
            public constructor(x: int, y: int) {
                this.x = x;
                this.y = y;
            }

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
       | Ok(AstModule(moduleName, klassList)) ->
            printfn ".module %s" (moduleName.ToString())
            printfn ""

            for AstKlass(klassDef, methodList) in klassList do
                printfn ".class[%s] %s {" (modifierToString klassDef.Modifiers) (klassDef.Name)

                for fieldDef in klassDef.Fields do
                    printfn "  .field[%s] %s: %s" 
                            (modifierToString fieldDef.Modifiers)
                            (fieldDef.Name)
                            (TypeIdent.getTypeName fieldDef.Type)

                printfn ""
                
                for AstMethod(methodDef, varList, body) in methodList do
                    let paramListStr =
                        methodDef.Parameters
                        |> List.map (fun (n, t) -> sprintf "%s: %s" n (TypeIdent.getTypeName t))
                        |> fun xs -> String.Join(", ", xs)

                    printfn "  .method[%s] %s(%s) -> %s {"
                            (modifierToString methodDef.Modifiers)
                            (methodDef.Name)
                            (paramListStr)
                            (TypeIdent.getTypeName methodDef.ReturnType)

                    printfn "    .local {"
                    varList
                    |> Seq.iteri (fun i (AstVariableDecl(n, t, _)) -> 
                                      printfn "      [%d] %s: %s" i n (TypeIdent.getTypeName t))

                    printfn "    }"

                    // generate bytecode
                    let codeBuf = CodeGen.createEmpty ()
                    compileStmt codeBuf (CodeGenContext.createContext -1) body |> ignore

                    printfn "    .body {"
                    codeBuf
                    |> Seq.map bytecodeToString
                    |> Seq.iteri (printfn "      [%d] %s")

                    printfn "    }"
                    
                    printfn "  }\n"

                printfn "}\n"

       | Error (ParsingError e) ->
            printfn "%s" e
       | Error (TranslationError e) ->
            printfn "%A" (e |> Seq.toList)


    Console.ReadKey() |> ignore

    0 // return an integer exit code
