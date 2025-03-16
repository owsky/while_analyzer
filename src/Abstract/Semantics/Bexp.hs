module Abstract.Semantics.Bexp (absBexpSemantics) where

import Abstract.Domain (AbstractDomain (..))
import Abstract.Semantics.Aexp (absAexpSemantics)
import Abstract.State (NonRelational)
import Abstract.Value (AbstractValue (..))
import Alarms (Alarms, mergeAlarms, updateAlarms)
import Ast.AexpAst (Aexp (..), AexpBinaryOp (..))
import Ast.BexpAst (AexpCompOp (..), Bexp (..), BoolBinaryOp (..), BoolUnaryOp (..), deMorgan)
import Data.Set qualified as Set
import Data.Text (Text)
import ProgramState (ProgramState (..))

-- | Abstract evaluation of Boolean expressions, defined by induction on their
-- | denotational semantics
absBexpSemantics ::
  (AbstractValue a, Show a, Eq a, Ord a) =>
  Bexp ->
  Int ->
  NonRelational Text a ->
  Alarms ->
  (NonRelational Text a, Alarms)
absBexpSemantics BTrue _ s alarms = (s, alarms)
absBexpSemantics BFalse _ _ alarms = (bottom, alarms)
absBexpSemantics (AexpComp op a1 a2) point s alarms = propagation op a1 a2 s point alarms
absBexpSemantics (BexpBinary op b1 b2) point s alarms =
  let
    (s', alarms') = absBexpSemantics b1 point s alarms
    (s'', alarms'') = absBexpSemantics b2 point s alarms
    finalAlarms = mergeAlarms alarms' alarms''
  in
    case op of
      And -> (s' `glb` s'', finalAlarms)
      Or -> (s' `lub` s'', finalAlarms)
absBexpSemantics (BexpUnary Not b) s point alarms = absBexpSemantics (deMorgan (BexpUnary Not b)) s point alarms

-- | Propagation algorithm for advanced abstract tests, based on HC4-revise
propagation ::
  (AbstractValue a, Show a, Eq a, Ord a) =>
  AexpCompOp -> -- arithmetic comparison operator
  Aexp -> -- left-hand side of the expression
  Aexp -> -- right-hand side of the expression
  NonRelational Text a -> -- input abstract state
  Int -> -- program point
  Alarms -> -- alarms map
  (NonRelational Text a, Alarms)
propagation op a1 a2 s point alarms =
  let
    -- forward evaluation of the two arithmetic subexpressions:
    (v1, e1) = absAexpSemantics a1 s
    (v2, e2) = absAexpSemantics a2 s
    errors = e1 `Set.union` e2
    -- compute the difference between v1 and v2 in order to rewrite the test as v1 - v2 op 0.
    -- This also represents the root of the tree of arithmetic operations
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

    -- intersect the root of the tree with the abstract value which makes the condition true
    rootIntersected = root `glb` trueValue

    -- propagate the new information obtained through the intersection
    -- of the root, to the children of the root
    (v1', v2') = backAbsBinary Sub (v1, v2, rootIntersected)

    -- propagate the refined information to both subtrees rooted at the children of the root
    leftS = backpropagate a1 v1' s
    rightS = backpropagate a2 v2' leftS
  in
    (rightS, updateAlarms alarms point errors)

-- Function which given an arithmetic expression, a refined result of the arithmetic operation and
-- an abstract state, it backpropagates the refined result through the backwards abstract operators
-- by induction on the denotational semantics of the arithmetic expressions
backpropagate ::
  (AbstractValue a, Show a, Eq a, Ord a) =>
  Aexp ->
  a ->
  NonRelational Text a ->
  NonRelational Text a
backpropagate (Var x) newVal s = update s x newVal
backpropagate (AexpUnary op e) newVal s =
  let
    (oldVal, _) = absAexpSemantics e s
    refinedVal = backAbsUnary op (oldVal, newVal)
  in
    backpropagate e refinedVal s
backpropagate (AexpBinary op e1 e2) newVal s =
  let
    (oldVal1, _) = absAexpSemantics e1 s
    (oldVal2, _) = absAexpSemantics e2 s
    (refinedVal1, refinedVal2) = backAbsBinary op (oldVal1, oldVal2, newVal)
    s1 = backpropagate e1 refinedVal1 s
  in
    backpropagate e2 refinedVal2 s1
backpropagate _ _ s = s