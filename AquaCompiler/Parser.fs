module Aqua.Parser

open Aqua.Language
open Aqua.Syntax
open FParsec

let kLongestCommentLength = 10000

let keywords =
    [ "true"; "false"; "if"; "else"; "while"; 
      "fun"; "return"; "unit"; "bool"; "int";
      "val"; "var"; ]

type SynExprSuffix =
    | MemberAccessSuffix of Range*string
    | InvocationSuffix of Range*SyntaxExpr list

module ParsecInstance =
    // [Basic Component] is atomic element in syntax
    // [Syntax Element] is coumpound element and would pull tailing spaces


    // [Basic Component] identifiers
    //

    let pIdent =
        let pkeyword = keywords |> List.map skipString |> choice
        let idOption = IdentifierOptions()

        notFollowedByL pkeyword "keyword" >>. identifier idOption

    // [Basic Component] literals
    //
    let pBool = 
        choice [stringReturn "true" true; stringReturn "false" false] 
        |>> BoolConst

    let pInt = 
        pint32 |>> IntConst

    let pLiteral =
        choice [pInt; pBool]

    // [Basic Component] whitespaces and comments
    //
    let pLineComment = 
        skipString "//" 
        .>> skipRestOfLine true 
        <?> "line comment"
    let pBlockComment = 
        skipString "/*" 
        .>> skipCharsTillString "*/" true kLongestCommentLength
        <?> "block comment"

    let pws = [ spaces1; pLineComment; pBlockComment; ] |> choice |> skipMany

    // Utilities
    //

    let pullSpace p =
        p .>> pws

    let pIdent_ws =
        pIdent |> pullSpace

    let pLiteral_ws =
        pLiteral |> pullSpace

    let skipChar_ws c = skipChar c |> pullSpace
    let skipString_ws s = skipString s |> pullSpace

    let charReturn_ws c result = charReturn c result |> pullSpace
    let stringReturn_ws s result = stringReturn s result |> pullSpace

    // (woundn't consume tailing spaces)
    let sepByCharDelimiter =
        fun c p -> sepBy p (skipChar_ws c)

    let sepByComma = 
        fun p -> sepByCharDelimiter ',' p

    // (woundn't consume tailing spaces)
    let betweenCharDelimiter open' close =
        between (skipChar_ws open') (skipChar close)

    let betweenParathe = fun p -> betweenCharDelimiter '(' ')' p
    let betweenBracket = fun p -> betweenCharDelimiter '[' ']' p
    let betweenBrace   = fun p -> betweenCharDelimiter '{' '}' p

    // assumes opString does not affect line number
    let adjustPosition opString (pos: Position) =
        let offset = opString |> String.length |> (~-) |> int64

        Position(pos.StreamName, pos.Index + offset,
                 pos.Line, pos.Column + offset)

    let makeRange (posStart: Position) (posEnd: Position) =
        { StartIndex    = int <| posStart.Index
          Length        = int <| posEnd.Index - posStart.Index
          StartLine     = int <| posStart.Line
          StartColumn   = int <| posStart.Column }

    let mergeRange first last =
        assert (first.StartIndex + first.Length <= last.StartIndex)

        { StartIndex    = first.StartIndex 
          Length        = last.StartIndex - first.StartIndex + last.Length 
          StartLine     = first.StartLine
          StartColumn   = first.StartColumn }

    let withRange f p =
        pipe3 getPosition p getPosition
              (fun posStart result posEnd ->
                   f (makeRange posStart posEnd) result)

    let withRange1 ctor p =
        p |> withRange (fun rg (x1) -> ctor (rg, x1))
    let withRange2 ctor p =
        p |> withRange (fun rg (x1, x2) -> ctor (rg, x1, x2))
    let withRange3 ctor p =
        p |> withRange (fun rg (x1, x2, x3) -> ctor (rg, x1, x2, x3))
    let withRange4 ctor p =
        p |> withRange (fun rg (x1, x2, x3, x4) -> ctor (rg, x1, x2, x3, x4))

    // [Syntax Element] type
    //
    let pType, pTypeImpl = createParserForwardedToRef()

    let pSystemType =
        [ "unit", Unit; "bool", Bool; "int", Int;]
        |> List.map (fun (text, type') -> stringReturn text type')
        |> choice
        |> withRange1 Syn_SystemType

    let pCustomType =
        pIdent |> withRange1 Syn_UserType
    
    let pMaybeFunctionType =
        let pAtomicType = (pSystemType <|> pCustomType) |> pullSpace
        let pReturnType = (skipString_ws "->" >>. pType) |> pullSpace

        let makeMultiMapFunctionType posStart xs (y: SyntaxType) =
            let first = makeRange posStart posStart
            let last = y.Range
            let rg = mergeRange first last

            Syn_FunctionType(rg, xs, y)

        let makeSingleMapFunctionType (x: SyntaxType) (y: SyntaxType) =
            let rg = mergeRange x.Range y.Range

            Syn_FunctionType(rg, List.singleton x, y)

        let pWithParamsInParathe =
            tuple3 getPosition (pType |> sepByComma |> betweenParathe |> pullSpace) (many pReturnType)
            >>= function
                | _, [x], []   -> preturn x
                | p, xs, y::ys -> preturn (List.fold makeSingleMapFunctionType (makeMultiMapFunctionType p xs y) ys)
                | _, [], []    -> fail "empty type is not allowed"
                | _, _, []     -> fail "type list is not allowed"

        let pWithParamsExposed =
            pipe2 pAtomicType (many pReturnType) (List.fold makeSingleMapFunctionType)

        pWithParamsInParathe <|> pWithParamsExposed

    do pTypeImpl :=
        pMaybeFunctionType <?> "type"

    let pTypeAnnot =
        (skipChar_ws ':') >>. pType <?> "type annotation"
    let pTypeAnnotOpt =
        opt pTypeAnnot

    // [Syntax Element] expression
    //
    let pLiteralExpr =
        pLiteral 
        |> withRange1 Syn_LiteralExpr
        |> pullSpace

    let pNamedExpr =
        pIdent 
        |> withRange1 Syn_NameAccessExpr
        |> pullSpace

    let pExpr =
        let opp = OperatorPrecedenceParser()

        let pAtomicExpr = 
            choice [ pLiteralExpr; pNamedExpr; opp.ExpressionParser |> betweenParathe |> pullSpace ]
        
        let pMemberAccessSuffix =
            let pDot = skipChar_ws '.'
            let pName = pIdent |> withRange1 MemberAccessSuffix

            pDot >>. pName |> pullSpace

        let pInvocationSuffix =
            opp.ExpressionParser 
            |> sepByComma 
            |> betweenParathe
            |> withRange1 InvocationSuffix
            |> pullSpace

        let pExprWithSuffix = 
            let exprSuffix = pMemberAccessSuffix <|> pInvocationSuffix

            pipe2 pAtomicExpr (many exprSuffix)
                  (List.fold (fun expr suffix -> 
                                  match suffix with
                                  | MemberAccessSuffix(rg, name) -> Syn_MemberAccessExpr(rg, expr, name)
                                  | InvocationSuffix(rg, args) -> Syn_InvocationExpr(rg, expr, args)))

        opp.TermParser <- pExprWithSuffix

        let assocLeft = Associativity.Left
        let assocRight = Associativity.Right

        let makeTypePostfixOp ctor opString assoc prec =
            let pAfterString =
                tuple2 (getPosition |> pullSpace) (pType |>> Some)

            PostfixOperator(opString, pAfterString, prec, assoc, (),
                            fun (pos, type') value ->
                                let rg = makeRange (adjustPosition opString pos) pos
                                ctor (rg, value, Option.get type')) 
                :> Operator<_, _, _>

        // workaround: some expression may need to parse a succeeding type
        let makeBinaryOp opType opString assoc prec =
            let pAfterString =
                tuple2 (getPosition |> pullSpace) (preturn None)

            InfixOperator(opString, pAfterString, prec, assoc, (),
                          fun (pos, _) lhs rhs -> 
                              let rg = makeRange (adjustPosition opString pos) pos
                              Syn_BinaryExpr(rg, opType, lhs, rhs))
                :> Operator<_, _, _>

        let systemOps =
            [ // assignment
              [ makeBinaryOp Op_Assign "=" assocRight ]

              // disjunction
              [ makeBinaryOp Op_Disjunction "||" assocLeft ]

              // conjunction
              [ makeBinaryOp Op_Conjunction "&&" assocLeft ]

              // equality
              [ makeBinaryOp Op_Equal "==" assocLeft
                makeBinaryOp Op_NotEqual "!=" assocLeft ]

              // comparison
              [ makeBinaryOp Op_Greater ">" assocLeft 
                makeBinaryOp Op_Less "<" assocLeft
                makeBinaryOp Op_GreaterEq ">=" assocLeft
                makeBinaryOp Op_LessEq "<=" assocLeft ]

              // type check
              [ makeTypePostfixOp Syn_TypeCheckExpr "is" false ]

              // bitwise op
              [ makeBinaryOp Op_BitwiseAnd "&" assocLeft 
                makeBinaryOp Op_BitwiseOr "|" assocLeft
                makeBinaryOp Op_BitwiseXor "^" assocLeft ]

              // additive
              [ makeBinaryOp Op_Plus "+" assocLeft
                makeBinaryOp Op_Minus "-" assocLeft ]

              // multiplicative
              [ makeBinaryOp Op_Asterisk "*" assocLeft
                makeBinaryOp Op_Slash "/" assocLeft
                makeBinaryOp Op_Modulus "%" assocLeft ]

              // type cast
              [ makeTypePostfixOp Syn_TypeCastExpr "as" false ]
            ]
        
        systemOps
        |> List.mapi (fun i fs -> fs |> List.map (fun f -> f (i+1)))
        |> List.collect id
        |> List.iter (fun op -> opp.AddOperator(op))

        opp.ExpressionParser <?> "expression"

    // [Syntax Element] statement
    //
    let pStmt, pStmtImpl = createParserForwardedToRef()

    let pExpressionStmt =
        pExpr |> withRange1 Syn_ExpressionStmt

    let pVarDeclStmt =
        let pkeyword = 
            (stringReturn_ws "val" Readonly) <|>
            (stringReturn_ws "var" Mutable)
        let pname = pIdent_ws
        let ptype = pTypeAnnotOpt
        let pinit = skipChar_ws '=' >>. pExpr
    
        tuple4 pkeyword pname ptype pinit |> withRange4 Syn_VarDeclStmt

    let pChoiceStmt =
        let pif = skipString_ws "if"
        let pelse = skipString_ws "else"
        let ptest = pExpr |> betweenParathe |> pullSpace
        let pposi = pStmt
        let pnega = opt (pelse >>. pStmt)

        (pif >>. (tuple3 ptest pposi pnega)) |> withRange3 Syn_ChoiceStmt

    let pWhileStmt =
        let pwhile = skipString_ws "while"
        let ptest = pExpr |> betweenParathe |> pullSpace
        let pbody = pStmt

        (pwhile >>. (tuple2 ptest pbody)) |> withRange2 Syn_WhileStmt

    let pControlFlowStmt =
        choice [stringReturn_ws "break" Break; stringReturn_ws "continue" Continue]
        |> withRange1 Syn_ControlFlowStmt

    let pReturnStmt =
        let preturn = skipString_ws "return"
        let pvalue = opt pExpr

        (preturn >>. pvalue) |> withRange1 Syn_ReturnStmt

    let pCompoundStmt =
        many pStmt 
        |> betweenBrace 
        |> withRange1 Syn_CompoundStmt
        |> pullSpace

    do pStmtImpl :=
        choiceL [
            pCompoundStmt;
            pChoiceStmt;
            pWhileStmt;
            pVarDeclStmt;
            pControlFlowStmt;
            pReturnStmt;
            pExpressionStmt; ] "statement"
        .>> optional (skipChar_ws ';')

    // declaration
    //
    let pModuleIdent =
        sepBy1 pIdent (skipChar_ws '.') 
        |>> (List.toArray >> (fun x -> ModuleIdent(x)))
        |> pullSpace

    let pModule =
        skipString_ws "module" >>. pModuleIdent 
        |>> ModuleDecl
        <?> "module declaration"

    let pImport =
        skipString_ws "import" >>. pModuleIdent 
        |>> ImportDecl
        <?> "import declaration"

    let pFunc =
        let pfun = skipString_ws "fun"
        let pname = pIdent_ws
        let pdeclarator = 
            let paramList =
                pIdent_ws .>>. pTypeAnnot
                |> sepByComma
                |> betweenParathe
                |> pullSpace

            let retType =
                skipString_ws "->" >>. pType

            paramList .>>. retType |>> FunctionDeclarator

        let pbody = pCompoundStmt
    
        pfun >>. (tuple3 pname pdeclarator pbody) 
        |>> (FunctionDecl >> GD_Function)
        <?> "function declaration"

    let pKlass =
        skipString_ws "class" >>. pIdent_ws
        |>> (KlassDecl >> GD_Klass)
        <?> "class declaration"

    // code page
    //

    // explicitly annotate the type to avoid generic value
    let (pCodePage: Parser<_, unit>) =
        let pPageContent =
            pipe3 (pModule) (many pImport) ([pFunc; pKlass] |> choice |> many)
                  (fun moduleInfo imports decls ->
                       { ModuleInfo = moduleInfo;
                         Imports    = imports;
                         Functions  = decls |> List.choose (function | GD_Function(x) -> Some x | _ -> None);
                         Klasses    = decls |> List.choose (function | GD_Klass(x)    -> Some x | _ -> None) })

        pws >>. pPageContent .>> eof

// interface
//
type Result =
    | Success of CodePage
    | Failure of string

let parseCodePage s =
    match runParserOnString ParsecInstance.pCodePage () "" s with
    | ParserResult.Success(result, _, _) -> Success result
    | ParserResult.Failure(error, _, _) -> Failure error