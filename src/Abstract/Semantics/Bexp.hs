module Abstract.Semantics.Bexp (absBexpSemantics) where

import Abstract.Domain (AbstractDomain (..))
import Abstract.ErrorProductState (ErrorProductState (..))
import Abstract.Semantics.Aexp (absAexpSemantics)
import Abstract.State (NonRelational)
import Abstract.Value (AbstractValue (..))
import Ast.AexpAst (Aexp (..), AexpBinaryOp (..))
import Ast.BexpAst (AexpCompOp (..), Bexp (..), BoolBinaryOp (..), BoolUnaryOp (..), deMorgan)
import Domains.Error (Error)
import State (State (..))

-- | Abstract evaluation of Boolean expressions, defined by induction on their
-- | denotational semantics
absBexpSemantics ::
  (AbstractValue a, Show a, Eq a, Ord a, AbstractDomain (NonRelational Int Error)) =>
  Bexp ->
  Int ->
  ErrorProductState a ->
  ErrorProductState a
absBexpSemantics BTrue _ s = s
absBexpSemantics BFalse _ (ErrorProductState _ err) = ErrorProductState bottom err
absBexpSemantics (AexpComp op a1 a2) point s = propagation op a1 a2 s point
absBexpSemantics (BexpBinary op b1 b2) point s =
  let
    (ErrorProductState s' err') = absBexpSemantics b1 point s
    (ErrorProductState s'' err'') = absBexpSemantics b2 point s
  in
    case op of
      And -> ErrorProductState (s' `glb` s'') (err' `glb` err'')
      Or -> ErrorProductState (s' `lub` s'') (err' `lub` err'')
absBexpSemantics (BexpUnary Not b) s point = absBexpSemantics (deMorgan (BexpUnary Not b)) s point

-- | Propagation algorithm for advanced abstract tests, based on HC4-revise
propagation ::
  (AbstractValue a, Show a, Eq a, Ord a) =>
  AexpCompOp ->
  Aexp ->
  Aexp ->
  ErrorProductState a ->
  Int ->
  ErrorProductState a
propagation op a1 a2 s point =
  let
    -- forward evaluation of the two arithmetic subexpressions:
    (v1, e1) = absAexpSemantics a1 s
    (v2, e2) = absAexpSemantics a2 s
    errors = e1 `lub` e2
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
    s1 = backpropagate a1 v1' s
    (ErrorProductState vs2 es2) = backpropagate a2 v2' s1
  in
    ErrorProductState vs2 (update es2 point errors)

-- Function which given an arithmetic expression, a refined result of the arithmetic operation and
-- an abstract state, it backpropagates the refined result through the backwards abstract operators
-- by induction on the denotational semantics of the arithmetic expressions
backpropagate ::
  (AbstractValue a, Show a, Eq a, Ord a) =>
  Aexp ->
  a ->
  ErrorProductState a ->
  ErrorProductState a
backpropagate (Var x) newVal (ErrorProductState s err) = ErrorProductState (update s x newVal) err
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