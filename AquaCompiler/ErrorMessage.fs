﻿module Aqua.ErrorMessage

open Aqua.Compiler

let createErrorMessage rg msg =
    { ReferenceRange = rg; Message = msg }

let invalidModuleReference rg ident =
    sprintf "cannot import module %A" ident
    |> createErrorMessage rg

let invalidUserType rg typeName =
    sprintf "cannot find user type %s" typeName
    |> createErrorMessage rg

let invalidImpilicitConversion rg srcType destType =
    sprintf "cannot impilicitly convert %O to %O" srcType destType
    |> createErrorMessage rg

let invalidControlFlow rg ctrl =
    sprintf "incorrectly placed control flow %O" ctrl
    |> createErrorMessage rg

let invalidInstanceReference rg =
    sprintf "this cannot be referred in static functions"
    |> createErrorMessage rg

let invalidVariableReference rg name =
    sprintf "variable %s cannot be found" name
    |> createErrorMessage rg

let invalidFieldReference rg typeName fieldName =
    sprintf "type %s not has member %s" typeName fieldName
    |> createErrorMessage rg

let invalidExpressionCall rg argTypeList =
    sprintf "expression cannot be called with %O" argTypeList
    |> createErrorMessage rg

let invalidFunctionCall rg name argTypeList =
    sprintf "function %s that takes %O cannot be found" name argTypeList
    |> createErrorMessage rg

let invalidFunctionResolve rg name argTypeList =
    sprintf "function %s that takes %O cannot be resolved" name argTypeList
    |> createErrorMessage rg

let invalidBinaryOperation rg binOp lhsType rhsType =
    sprintf "binary operation %O between %O and %O is undefined" binOp lhsType rhsType
    |> createErrorMessage rg

let invalidAssignment rg =
    sprintf "expression is not assignable(either declared readonly or a temporary value)"
    |> createErrorMessage rg


let invalidExpressionStmt rg =
    "only assignment or invocation expression can be a statement"
    |> createErrorMessage rg

let invalidVariableDecl rg name =
    sprintf "variable %s has already been declared in the scope" name
    |> createErrorMessage rg