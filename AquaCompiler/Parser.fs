module Aqua.Parser

open Aqua.Language
open Aqua.Syntax
open FParsec

let kLongestCommentLength = 10000

let keywords =
    [ "true"; "false"; "if"; "else"; "while"; 
      "fun"; "return"; "unit"; "bool"; "int";
      "val"; "var"; ]

module ParsecInstance =

    // whitespaces and comments
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

    // utils
    //

    let skipChar_ws c = skipChar c .>> pws
    let skipString_ws s = skipString s .>> pws

    let charReturn_ws c result = charReturn c result .>> pws
    let stringReturn_ws s result = stringReturn s result .>> pws

    //
    let sepByCharDelimiter =
        fun c p -> sepBy p (skipChar_ws c)

    let sepByComma = 
        fun p -> sepByCharDelimiter ',' p

    //
    let betweenCharDelimiter open' close =
        between (skipChar_ws open') (skipChar_ws close)

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

    // identifiers
    //
    let pSimpleIdentifier =
        let pkeyword = keywords |> List.map skipString |> choice
        let idOption = IdentifierOptions()

        notFollowedByL pkeyword "keyword" >>. identifier idOption

    let pIdent =
        pSimpleIdentifier .>> pws

    // literals
    //
    let pBool = 
        choice [stringReturn_ws "true" true; stringReturn_ws "false" false] 
        |>> BoolConst

    let pInt = 
        pint32 .>> pws |>> IntConst

    let pLiteral =
        choice [pInt; pBool]

    // type
    //
    let pType, pTypeImpl = createParserForwardedToRef()

    let pSystemType =
        [ "unit", Unit; "bool", Bool; "int", Int;]
        |> List.map (fun (text, type') -> stringReturn_ws text (SystemType type'))
        |> choice

    let pCustomType =
        pIdent |>> UserType
    
    let pMaybeFunctionType =
        let pAtomicType = pSystemType <|> pCustomType
        let pReturnType = skipString_ws "->" >>. pType

        let makeFunctionType src ret = 
            FunctionSignature(List.singleton src, ret) |> FunctionType

        let pWithParamsInParathe =
            (pType |> sepByComma |> betweenParathe) .>>. (many pReturnType)
            >>= function
                | [x], []  -> preturn x
                | t, x::xs -> preturn (List.fold makeFunctionType (FunctionType <| FunctionSignature(t, x)) xs)
                | [], []   -> fail "empty type is not allowed"
                | _, []    -> fail "type list is not allowed"

        let pWithParamsExposed =
            pipe2 pAtomicType (many pReturnType) (List.fold makeFunctionType)

        pWithParamsInParathe <|> pWithParamsExposed

    do pTypeImpl :=
        pMaybeFunctionType <?> "type"

    let pTypeAnnot =
        (skipChar_ws ':') >>. pType <?> "type annotation"
    let pTypeAnnotOpt =
        opt pTypeAnnot

    // expression
    //
    let pLiteralExpr =
        pLiteral |> withRange1 LiteralExpr

    let pNamedExpr =
        pIdent |> withRange1 NamedExpr

    let pExpr =
        let opp = OperatorPrecedenceParser()

        let pAtomicExpr = 
            choice [ pLiteralExpr; pNamedExpr; betweenParathe opp.ExpressionParser ]
        
        // TODO: add member access suffix
        let pInvocationExpr = 
            let callSuffix = opp.ExpressionParser |> sepByComma |> betweenParathe

            pipe2 pAtomicExpr (many (tuple3 getPosition callSuffix getPosition))
                  (List.fold (fun expr (posStart, args, posEnd) -> 
                                  let rg = makeRange posStart posEnd
                                  InvocationExpr(rg, expr, args)))

        opp.TermParser <- pInvocationExpr

        let assocLeft = Associativity.Left
        let assocRight = Associativity.Right

        let makeTypePostfixOp ctor opString assoc prec =
            let pAfterString =
                tuple3 getPosition (pws >>. pType |>> Some) getPosition

            PostfixOperator(opString, pAfterString, prec, assoc, (),
                            fun (posStart, type', posEnd) value ->
                                let rg = makeRange (adjustPosition opString posStart) posEnd
                                ctor (rg, value, Option.get type')) 
            :> Operator<_, _, _>

        // workaround: some expression may need to parse a succeeding type
        let makeBinaryOp opType opString assoc prec =
            let pAfterString =
                tuple3 getPosition (pws >>. preturn None) getPosition

            InfixOperator(opString, pAfterString, prec, assoc, (),
                          fun (posStart, _, posEnd) lhs rhs -> 
                              let rg = makeRange (adjustPosition opString posStart) posEnd
                              BinaryExpr(rg, opType, lhs, rhs))
            :> Operator<_, _, _>

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
          [ makeTypePostfixOp TypeCheckExpr "is" false ]
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
          [ makeTypePostfixOp TypeCastExpr "as" false ]
        ]
        |> List.mapi (fun i fs -> fs |> List.map (fun f -> f (i+1)))
        |> List.collect id
        |> List.iter (fun op -> opp.AddOperator(op))

        opp.ExpressionParser <?> "expression"

    // statement
    //
    let pStmt, pStmtImpl = createParserForwardedToRef()

    let pExpressionStmt =
        pExpr |> withRange1 ExpressionStmt

    let pVarDeclStmt =
        let pkeyword = 
            (stringReturn_ws "val" Readonly) <|>
            (stringReturn_ws "var" Mutable)
        let pname = pIdent
        let ptype = pTypeAnnotOpt
        let pinit = skipChar_ws '=' >>. pExpr
    
        tuple4 pkeyword pname ptype pinit |> withRange4 VarDeclStmt

    let pChoiceStmt =
        let pif = skipString_ws "if"
        let pelse = skipString_ws "else"
        let ptest = betweenParathe pExpr
        let pposi = pStmt
        let pnega = opt (pelse >>. pStmt)

        pif >>. (tuple3 ptest pposi pnega) |> withRange3 ChoiceStmt

    let pWhileStmt =
        let pwhile = skipString_ws "while"
        let ptest = betweenParathe pExpr
        let pbody = pStmt

        pwhile >>. (tuple2 ptest pbody) |> withRange2 WhileStmt

    let pControlFlowStmt =
        choice [stringReturn_ws "break" Break; stringReturn_ws "continue" Continue]
        |> withRange1 ControlFlowStmt

    let pReturnStmt =
        let preturn = skipString_ws "return"
        let pvalue = opt pExpr

        preturn >>. pvalue |> withRange1 ReturnStmt

    let pCompoundStmt =
        many pStmt |> betweenBrace |> withRange1 CompoundStmt

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
        sepBy1 pSimpleIdentifier (skipChar '.') .>> pws
        |>> (List.toArray >> (fun x -> ModuleIdent(x)))

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
        let pname = pIdent
        let psignature = 
            let paramList =
                pIdent .>>. pTypeAnnot
                |> sepByComma
                |> betweenParathe
            let retType =
                skipString_ws "->" >>. pType

            paramList .>>. retType |>> FunctionDeclarator

        let pbody = pCompoundStmt
    
        pfun >>. (tuple3 pname psignature pbody) 
        |>> (FunctionDecl >> GD_Function)
        <?> "function declaration"

    let pKlass =
        skipString_ws "class" >>. pIdent 
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
                         Klasses    = decls |> List.choose (function | GD_Klass(x) -> Some x | _ -> None) })

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