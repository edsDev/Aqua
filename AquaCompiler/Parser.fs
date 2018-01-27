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

    // identifiers
    //
    let pIdent =
        let pkeyword = keywords |> List.map skipString |> choice
        let idOption = IdentifierOptions()

        notFollowedByL pkeyword "keyword" >>. identifier idOption .>> pws
        |>> Identifier

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

        let makeFunctionType src ret = FunctionType(List.singleton src, ret)

        let pWithParamsInParathe =
            (pType |> sepByComma |> betweenParathe) .>>. (many pReturnType)
            >>= function
                | [x], []  -> preturn x
                | t, x::xs -> preturn (List.fold makeFunctionType (FunctionType(t, x)) xs)
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
        pTypeAnnot <|>% TBD

    // expression
    //
    let pLiteralExpr =
        pLiteral |>> LiteralExpr

    let pNamedExpr =
        pIdent |>> NamedExpr

    let pExpr =
        let translateBinaryOp = function
            | "+" -> Op_Plus
            | "-" -> Op_Minus
            | "*" -> Op_Asterisk
            | "/" -> Op_Slash
            | "%" -> Op_Modulus
            | ">" -> Op_Greater
            | ">=" -> Op_GreaterEq
            | "<" -> Op_Less
            | "<=" -> Op_LessEq
            | "==" -> Op_Equal
            | "!=" -> Op_NotEqual
            | "&" -> Op_BitwiseAnd
            | "|" -> Op_BitwiseOr
            | "^" -> Op_BitwiseXor
            | "&&" -> Op_LogicalAnd
            | "||" -> Op_LogicalOr
            | _ -> failwith "not a valid BinaryOp"

        let opp = OperatorPrecedenceParser()

        let pAtomicExpr = 
            choice [ pLiteralExpr; pNamedExpr; betweenParathe opp.ExpressionParser ]
        
        // TODO: add member access suffix
        let pInvocationExpr = 
            pipe2 pAtomicExpr (opp.ExpressionParser |> sepByComma |> betweenParathe |> many)
                  (List.fold (fun expr args -> InvocationExpr(expr, args)))
            
        opp.TermParser <- pInvocationExpr

        let assocLeft = Associativity.Left
        let assocRight = Associativity.Right

        // workaround: some expression may need to parse a succeeding type
        let addInfixOp prefix prec assoc =
            let op = InfixOperator(prefix, pws >>. (preturn TBD), prec, assoc, (),
                                   fun _ lhs rhs -> BinaryExpr(translateBinaryOp prefix, lhs, rhs))
            opp.AddOperator(op)

        let addTypePostfixOp prefix prec assoc ctor =
            let op = PostfixOperator(prefix, pws >>. pType, prec, assoc, (),
                                     fun type' value -> ctor (value, type'))
            opp.AddOperator(op)

        // disjunction 1
        addInfixOp "||" 1 assocLeft
        // conjunction 2
        addInfixOp "&&" 2 assocLeft
        // equality 3
        addInfixOp "==" 3 assocLeft
        addInfixOp "!=" 3 assocLeft
        // comprison 4
        addInfixOp ">" 4 assocLeft
        addInfixOp "<" 4 assocLeft
        addInfixOp ">=" 4 assocLeft
        addInfixOp "<=" 4 assocLeft
        // type check 5
        addTypePostfixOp "is" 5 false TypeCheckExpr
        // bitwise operation
        addInfixOp "&" 6 assocLeft
        addInfixOp "|" 6 assocLeft
        addInfixOp "^" 6 assocLeft
        // additive 7
        addInfixOp "+" 7 assocLeft
        addInfixOp "-" 7 assocLeft
        // multiplicative 8
        addInfixOp "*" 8 assocLeft
        addInfixOp "/" 8 assocLeft
        addInfixOp "%" 8 assocLeft
        // type conversion 8
        addTypePostfixOp "as" 9 false TypeConvertExpr

        opp.ExpressionParser <?> "expression"

    // statement
    //
    let pStmt, pStmtImpl = createParserForwardedToRef()

    let pExpressionStmt =
        pExpr |>> ExpressionStmt

    let pVarDeclStmt =
        let pkeyword = 
            (stringReturn_ws "val" Readonly) <|>
            (stringReturn_ws "var" Mutable)
        let pname = pIdent
        let ptype = pTypeAnnotOpt
        let pinit = skipChar_ws '=' >>. pExpr
    
        tuple4 pkeyword pname ptype pinit |>> VarDeclStmt

    let pChoiceStmt =
        let pif = skipString_ws "if"
        let pelse = skipString_ws "else"
        let ptest = betweenParathe pExpr
        let pposi = pStmt
        let pnega = opt (pelse >>. pStmt)

        pif >>. (tuple3 ptest pposi pnega) |>> ChoiceStmt

    let pWhileStmt =
        let pwhile = skipString_ws "while"
        let ptest = betweenParathe pExpr
        let pbody = pStmt

        pwhile >>. (tuple2 ptest pbody) |>> WhileStmt

    let pControlFlowStmt =
        choice [stringReturn_ws "break" Break; stringReturn_ws "continue" Continue]
        |>> ControlFlowStmt

    let pReturnStmt =
        let preturn = skipString_ws "return"
        let pvalue = opt pExpr

        preturn >>. pvalue |>> ReturnStmt

    let pCompoundStmt =
        many pStmt |> betweenBrace |>> CompoundStmt

    do pStmtImpl :=
        choiceL [
            pVarDeclStmt;
            pChoiceStmt;
            pWhileStmt;
            pControlFlowStmt;
            pReturnStmt;
            pCompoundStmt;
            pExpressionStmt; ] "statement"
        .>> optional (skipChar_ws ';')

    // declaration
    //
    let pModule =
        skipString_ws "module" >>. pIdent 
        |>> ModuleDecl
        <?> "module declaration"

    let pImport =
        skipString_ws "import" >>. pIdent 
        |>> ImportDecl
        <?> "import declaration"

    let pFunc =
        let pfun = skipString_ws "fun"
        let pname = pIdent
        let psignature = 
            let paramList =
                pIdent .>>. pTypeAnnot |>> NameTypePair
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