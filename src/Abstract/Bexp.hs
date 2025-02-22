module Abstract.Bexp (absBexpSemantics) where

import Abstract.Aexp (absAexpSemantics)
import Abstract.Domain (AbstractDomain (..))
import Abstract.State (AbstractState, NonRelational)
import Abstract.Value (AbstractValue (..))
import Ast.AexpAst (Aexp (..), AexpBinaryOp (..))
import Ast.BexpAst (AexpCompOp (..), Bexp (..), BoolBinaryOp (..), BoolUnaryOp (..), deMorgan)
import Data.Text (Text)
import State (State (..))

-- | Abstract evaluation of Boolean expressions, defined by induction on their
-- | denotational semantics
absBexpSemantics ::
  (AbstractValue a, AbstractState (NonRelational Text a), Show a, Eq a, Ord a) =>
  Bexp ->
  NonRelational Text a ->
  NonRelational Text a
absBexpSemantics BTrue s = s
absBexpSemantics BFalse _ = bottom
absBexpSemantics (AexpComp op a1 a2) s = propagation op a1 a2 s
absBexpSemantics (BexpBinary op b1 b2) s = case op of
  And -> absBexpSemantics b1 s `glb` absBexpSemantics b2 s
  Or -> absBexpSemantics b1 s `lub` absBexpSemantics b2 s
absBexpSemantics (BexpUnary Not b) s = absBexpSemantics (deMorgan (BexpUnary Not b)) s

-- | Propagation algorithm for advanced abstract tests, based on HC4-revise
propagation ::
  (AbstractValue a, Show a, Eq a, Ord a) => AexpCompOp -> Aexp -> Aexp -> NonRelational Text a -> NonRelational Text a
propagation op a1 a2 s =
  let
    -- forward evaluation of the two arithmetic subexpressions:
    v1 = absAexpSemantics a1 s
    v2 = absAexpSemantics a2 s
    -- compute the difference between v1 and v2 in order to rewrite the test
    -- as v1 - v2 op 0. This also represents the root of the tree of arithmetic
    -- operations
    root = absBinary Sub v1 v2

    -- get the abstract value for which the test, rewritten in terms of
    -- comparison with the constant 0, is true
    trueValue = case op of
      Eq -> absConst 0
      Neq -> negative `lub` positive
      Lt -> negative
      Le -> nonPositive
      Gt -> positive
      Ge -> nonNegative

    -- intersect the root of the tree with the abstract value which makes
    -- the condition true
    rootIntersected = root `glb` trueValue

    -- propagate the new information obtained through the intersection
    -- of the root, to the children of the root
    (v1', v2') = backAbsBinary Sub (v1, v2, rootIntersected)

    -- propagate the refined information to both subtrees rooted at the
    -- children of the root
    s1 = backpropagate a1 v1' s
    s2 = backpropagate a2 v2' s1
  in
    s2

-- Function which given an arithmetic expression, a refined result of the arithmetic operation and
-- an abstract state, it backpropagates the refined result through the backwards abstract operators
-- by induction on the denotational semantics of the arithmetic expressions
backpropagate :: (AbstractValue a, Show a, Eq a, Ord a) => Aexp -> a -> NonRelational Text a -> NonRelational Text a
backpropagate (Var x) newVal s = update s x newVal
backpropagate (AexpUnary op e) newVal s =
  let oldVal = absAexpSemantics e s
      refinedVal = backAbsUnary op (oldVal, newVal)
  in backpropagate e refinedVal s
backpropagate (AexpBinary op e1 e2) newVal s =
  let oldVal1 = absAexpSemantics e1 s
      oldVal2 = absAexpSemantics e2 s
      (refinedVal1, refinedVal2) = backAbsBinary op (oldVal1, oldVal2, newVal)
      s1 = backpropagate e1 refinedVal1 s
  in backpropagate e2 refinedVal2 s1
backpropagate _ _ s = s