module Batsh.Ast.Typed(module Poly, module Batsh.Ast.Typed) where

import Batsh.Ast.Poly as Poly
import Batsh.Token(LexPos)

data Type = TInt | TFloat | TString | TList

data TypeAnno = TypeAnno Type LexPos

type Literal = PLiteral TypeAnno

type LeftValue = PLeftValue TypeAnno

type UnaryOperator = PUnaryOperator TypeAnno

type BinaryOperator = PBinaryOperator TypeAnno

type Expression = PExpression TypeAnno

type Statement = PStatement TypeAnno

type TopLevel = PTopLevel TypeAnno

type Program = PProgram TypeAnno

nodeType :: AstNode a => a TypeAnno -> Type
nodeType node = typ where TypeAnno typ _ = annot node

nodePos :: AstNode a => a TypeAnno -> LexPos
nodePos node = pos where TypeAnno _ pos = annot node
