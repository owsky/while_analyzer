module Abstract.Semantics.Aexp (absAexpSemantics) where

import Abstract.State (NonRelational)
import Abstract.Value (AbstractValue (..))
import Alarms (AlarmFlag, AlarmType (..), Severity (..))
import Ast.AexpAst (Aexp (..), AexpBinaryOp (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import State (State (..))
import Prelude hiding (lookup)

-- | Abstract evaluation of arithmetic expressions, defined by induction on their denotational semantics
absAexpSemantics :: (AbstractValue a, Show a, Eq a, Ord a) => Aexp -> NonRelational Text a -> (a, Set AlarmFlag)
absAexpSemantics (Var v) s = (lookup s v, Set.empty)
absAexpSemantics (Val n) _ = (absConst n, Set.empty)
absAexpSemantics (AexpUnary op e) s =
  let (val, e') = absAexpSemantics e s
  in (absUnary op val, e')
absAexpSemantics (AexpBinary op a1 a2) s =
  let
    (val1, e1) = absAexpSemantics a1 s
    (val2, e2) = absAexpSemantics a2 s
    e' = e1 `Set.union` e2
    finalAlarm = case op == Div of
      False -> e'
      -- check for division by zero
      True -> case val2 of
        _ | val2 == absConst 0 -> Set.insert (DivByZero, Sure) e'
        _ | includesZero val2 -> Set.insert (DivByZero, Possible) e'
        _ -> e'
  in
    (absBinary op val1 val2, finalAlarm)
