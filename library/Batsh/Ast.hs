module Batsh.Ast(module Poly, module Batsh.Ast) where

import Batsh.Ast.Poly as Poly
import Batsh.Token(LexPos)

type AstAnnotation = LexPos

type Literal = PLiteral AstAnnotation

type LeftValue = PLeftValue AstAnnotation

type UnaryOperator = PUnaryOperator AstAnnotation

type BinaryOperator = PBinaryOperator AstAnnotation

type Expression = PExpression AstAnnotation

type Statement = PStatement AstAnnotation

type TopLevel = PTopLevel AstAnnotation

type Program = PProgram AstAnnotation

type Node = PNode AstAnnotation
