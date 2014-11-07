{-# LANGUAGE FlexibleInstances #-}
module Batsh.TypeCheck where

import qualified Batsh.Ast as Raw
import Batsh.Ast.Typed

class TypeCheckable a where
  typeCheck :: a -> b

--instance TypeCheckable Raw.Literal where
--  typeCheck literal = case literal of
--    Int num pos -> Int num ()
