open System
open Aqua.Language
open Aqua.Syntax
open Aqua.Parser
open Aqua.Compiler

// first pass: parse the source file
// second pass: enumerate and memorize declarations
// third pass: do type checking

let rec translateSyntaxType = function
    | SystemType(t) -> 
        SystemTypeStub(t)
    | UserType(Identifier(name)) -> 
        UserTypeStub(name)
    | FunctionType(paramTypes, retType) ->
        FunctionTypeStub <| FunctionSignature(paramTypes |> List.map translateSyntaxType, retType |> translateSyntaxType)
    | TBD ->
        failwith "type not yet determined"

let translateModuleDecl = function
    | ModuleDecl(Identifier(name)) -> ModuleId <| upcast Array.singleton name

let translateImportDecl = function
    | ImportDecl(Identifier(name)) -> ModuleId <| upcast Array.singleton name

let translateKlassDecl = function
    | KlassDecl(Identifier(name)) -> KlassDefinition(name, [], [])

let translateFunctionDecl = function
    | FunctionDecl(Identifier(name), FunctionDeclarator(paramTypes, retType), _) -> 
        let translatedParamTypes = 
            paramTypes |> List.map (function | NameTypePair(_, type') -> translateSyntaxType type')
        let translatedReturnType =
            retType |> translateSyntaxType
        
        FunctionDefinition(name, FunctionSignature(translatedParamTypes, translatedReturnType))

// TODO: add enum support
let extractModuleInfo syntaxTree =
    { ModuleName    = syntaxTree.ModuleInfo |> translateModuleDecl;
      ImportList    = syntaxTree.Imports    |> List.map translateImportDecl;
      EnumList      = [];
      KlassList     = syntaxTree.Klasses    |> List.map translateKlassDecl;
      FunctionList  = syntaxTree.Functions  |> List.map translateFunctionDecl; }

let createTranslationEnv thisModule =
    let enums =
        thisModule.EnumList
        |> Seq.map (fun (EnumDefinition(name, _) as def) -> name, EnumLookupItem(def))
    let klasses = 
        thisModule.KlassList 
        |> Seq.map (fun (KlassDefinition(name, _, _) as def) -> name, KlassLookupItem(def))
    let functions =
        thisModule.FunctionList
        |> Seq.map (fun (FunctionDefinition(name, _) as def) -> name, FunctionLookupItem(def)) 

    { TypeLookupTable = dict <| Seq.append enums klasses;
      MethodLookupTable = dict <| [];
      FunctionLookupTable = dict <| functions; }

let (|ExistImplicitConversion|_|) env destType srcType =
    Some ()
    
module ErrorMessage =
    let invalidImpilicitConversion srcType destType =
        sprintf "cannot impilicitly convert %O to %O" srcType destType

    let invalidControlFlow ctrl =
        sprintf "invalid control flow %O" ctrl

    let invalidVariableName name =
        sprintf "variable %O cannot be found" name

module FunctionTranslation =

    type VariableLookupItem =
        | VariableLookupItem of string*MutablityModifier*TypeStub

    type Context =
        { AllowControlFlow: bool;
          VariableLookup: Map<string, VariableLookupItem>;
          ExprTypeCache: Map<Expression, TypeStub>;
          ErrorMessages: string list; }

    let initContext paramList =
        { AllowControlFlow = false; 
          VariableLookup = paramList 
                           |> List.map (fun (NameTypePair((Identifier name), type')) -> name, VariableLookupItem(name, Readonly, translateSyntaxType type'))
                           |> Map.ofList;
          ExprTypeCache = Map.empty;
          ErrorMessages = []; }

    let enterLoopBody ctx =
        { ctx with AllowControlFlow = true }

    let exitLoopBody ctx =
        { ctx with AllowControlFlow = false }

    let declareVariable var ctx =
        let (VariableLookupItem(name, _, _)) = var
        { ctx with VariableLookup = ctx.VariableLookup |> Map.add name var }
    
    let cacheExprType expr type' ctx =
        { ctx with ExprTypeCache = ctx.ExprTypeCache |> Map.add expr type' }

    let appendError msg ctx =
        { ctx with ErrorMessages = msg::ctx.ErrorMessages }

    let checkFunction env func =
        let (FunctionDecl(Identifier(name), FunctionDeclarator(paramTypes, retType), body)) = func
        
        let retType = translateSyntaxType retType

        // expr -> ctx, type
        let rec checkExpr ctx expr =
            let newContext =
                match expr with
                | LiteralExpr(value) ->
                    match value with
                    | BoolConst(_) -> ctx |> cacheExprType expr BoolType
                    | IntConst(_) -> ctx |> cacheExprType expr IntType
                | NamedExpr(Identifier(name)) ->
                    match ctx.VariableLookup |> Map.tryFind name with
                    | Some(VariableLookupItem(_, _, t)) -> 
                        ctx 
                        |> cacheExprType expr t
                    | None ->
                        ctx 
                        |> cacheExprType expr UnitType 
                        |> appendError (ErrorMessage.invalidVariableName name)
                    
                | _ -> 
                    ctx |> cacheExprType expr UnitType

            newContext, newContext.ExprTypeCache |> Map.find expr

        // stmt -> ctx
        let ensureTypeConversion ctx srcType destType =
            match srcType with
            | ExistImplicitConversion env destType ->
                ctx
            | _ ->
                ctx |> appendError (ErrorMessage.invalidImpilicitConversion srcType destType)
    
        let ensureExprType ctx expr constraintType =
            let newContext, exprType = checkExpr ctx expr
            
            ensureTypeConversion newContext exprType constraintType

        let rec checkStmt ctx stmt =
            match stmt with
            | ExpressionStmt(expr) ->
                checkExpr ctx expr |> fst
            | VarDeclStmt(mut, Identifier(name), type', init) ->
                let newContext, exprType = checkExpr ctx init
                match type' with
                | TBD -> 
                    newContext 
                    |> declareVariable (VariableLookupItem(name, mut, exprType))
                | _ ->
                    let t = translateSyntaxType type'
                    ensureTypeConversion newContext exprType t
                    |> declareVariable (VariableLookupItem(name, mut, t))

            | ChoiceStmt(pred, posiBranch, negaBranch) ->
                let newContext = ensureExprType ctx pred BoolType
                
                [ Some posiBranch; negaBranch; ]
                |> List.choose id
                |> List.fold checkStmt newContext

            | WhileStmt(pred, body) ->
                ensureExprType ctx pred BoolType
                |> enterLoopBody
                |> (fun ctx -> checkStmt ctx body)
                |> exitLoopBody

            | ControlFlowStmt(ctrl) ->
                if ctx.AllowControlFlow then
                    ctx
                else
                    ctx |> appendError (ErrorMessage.invalidControlFlow ctrl)

            | ReturnStmt(maybeExpr) ->
                let newContext, exprType =
                    match maybeExpr with
                    | Some(expr) -> checkExpr ctx expr
                    | None       -> ctx, UnitType

                ensureTypeConversion newContext exprType retType

            | CompoundStmt(children) ->
                let result = List.fold checkStmt ctx children
                { result with VariableLookup = ctx.VariableLookup }
    
        checkStmt (initContext paramTypes) body



[<EntryPoint>]
let main argv =
    let sampleCode = """
        module mycode

        import std
        fun foo(x: int) -> bool {
            while(true) {break}
            if ((x==42) is bool) 
                return true
            else
                return false
        }

        // line comment
        fun sum(x: int, y: int) -> int {
            return x + /* block comment */ y
        }

        fun apply(f: (int, int) -> int , x: int, y: int) -> int {
            return f(x, y)
        }
    """
   
    // parse
    let codePage = 
        match parseCodePage sampleCode with
        | Success t -> t
        | Failure e -> failwith e

    let moduleInfo = extractModuleInfo codePage
    let env = createTranslationEnv moduleInfo

    codePage.Functions
    |> List.map (FunctionTranslation.checkFunction env)
    |> List.iter (printfn "%A\n\n")

    Console.ReadKey() |> ignore

    0 // return an integer exit code
