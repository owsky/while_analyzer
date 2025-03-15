module Abstract.Semantics.Aexp (absAexpSemantics) where

import Abstract.Domain (AbstractDomain (..))
import {-# SOURCE #-} Abstract.ErrorProductState (ErrorProductState (..))
import Abstract.Value (AbstractValue (..))
import Ast.AexpAst (Aexp (..), AexpBinaryOp (..))
import Data.Set (singleton)
import Domains.Error (Error (..), ErrorType (DivisionByZero))
import State (State (..))
import Prelude hiding (lookup)

-- | Abstract evaluation of arithmetic expressions, defined by induction on their denotational semantics
absAexpSemantics :: (AbstractValue a, Show a, Eq a, Ord a) => Aexp -> ErrorProductState a -> (a, Error)
absAexpSemantics (Var v) (ErrorProductState s _) = (lookup s v, bottom)
absAexpSemantics (Val n) _ = (absConst n, bottom)
absAexpSemantics (AexpUnary op e) s =
  let (val, e') = absAexpSemantics e s
  in (absUnary op val, e')
absAexpSemantics (AexpBinary op a1 a2) s =
  let
    (val1, e1) = absAexpSemantics a1 s
    (val2, e2) = absAexpSemantics a2 s
    newEs = e1 `lub` e2
    finalEs =
      if op == Div && includesZero val2
        then newEs `lub` Error (singleton DivisionByZero)
        else newEs
  in
    (absBinary op val1 val2, finalEs)
