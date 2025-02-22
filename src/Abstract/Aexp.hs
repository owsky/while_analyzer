module Abstract.Aexp (absAexpSemantics) where

import Abstract.State (NonRelational)
import Abstract.Value (AbstractValue (..))
import Ast.AexpAst (Aexp (..))
import Data.Text (Text)
import State (State (..))
import Prelude hiding (lookup)

-- | Abstract evaluation of arithmetic expressions, defined by induction on their
-- | denotational semantics
absAexpSemantics :: (AbstractValue a, Show a, Eq a, Ord a) => Aexp -> NonRelational Text a -> a
absAexpSemantics (Var v) s = lookup s v
absAexpSemantics (Val n) _ = absConst n
absAexpSemantics (AexpUnary op e) s = absUnary op (absAexpSemantics e s)
absAexpSemantics (AexpBinary op a1 a2) s = absBinary op (absAexpSemantics a1 s) (absAexpSemantics a2 s)