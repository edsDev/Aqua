module Aqua.ErrorMessage

open Aqua.Compiler

let createErrorMessage rg msg =
    { ReferenceRange = rg; Message = msg }

let invalidImpilicitConversion rg srcType destType =
    sprintf "cannot impilicitly convert %O to %O" srcType destType
    |> createErrorMessage rg

let invalidControlFlow rg ctrl =
    sprintf "invalid control flow %O" ctrl
    |> createErrorMessage rg

let invalidVariableReference rg name =
    sprintf "variable %O cannot be found" name
    |> createErrorMessage rg

let invalidExpressionCall rg argTypeList =
    sprintf "expression cannot be called with %O" argTypeList
    |> createErrorMessage rg

let invalidFunctionCall rg name argTypeList =
    sprintf "function %O that takes %O cannot be found" name argTypeList
    |> createErrorMessage rg

let invalidFunctionResolve rg name argTypeList =
    sprintf "function %O that takes %O cannot be resolved" name argTypeList
    |> createErrorMessage rg

let invalidBinaryOperation rg binOp lhsType rhsType =
    sprintf "binary operation %O between %O and %O is undefined" binOp lhsType rhsType
    |> createErrorMessage rg


let invalidExpressionStmt rg =
    "only assignment or invocation expression can be a statement"
    |> createErrorMessage rg