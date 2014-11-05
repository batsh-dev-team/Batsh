module Batsh.Ast(module Poly, module Batsh.Ast) where

import Batsh.Ast.Poly as Poly
import Batsh.Lexer(LexPos)

type Annotation = ()

type Literal = PLiteral Annotation

type LeftValue = PLeftValue Annotation

type UnaryOperator = PUnaryOperator Annotation

type BinaryOperator = PBinaryOperator Annotation

type Expression = PExpression Annotation

type Statement = PStatement Annotation

type TopLevel = PTopLevel Annotation

type Program = PProgram Annotation
