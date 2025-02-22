module Abstract.While (absWhileSemantics) where

import Abstract.Bexp (absBexpSemantics)
import Abstract.Domain (AbstractDomain (..))
import Abstract.State (AbstractState (..), NonRelational (..))
import Abstract.Value (AbstractValue)
import Ast.BexpAst (Bexp (..), BoolUnaryOp (..))
import Ast.WhileAst (While (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Interval.ExtendedInt (ExtendedInt)
import Utils (decrMaybe)

-- | Abstract evaluation of While programs, defined by induction on their
-- | denotational semantics
absWhileSemantics ::
  (AbstractValue a, AbstractState (NonRelational Text a), Show a, Eq a, Ord a) =>
  Set ExtendedInt -> -- numeric thresholds to use during widening
  Maybe Int -> -- widening delay, if Nothing widening is not used
  Maybe Int -> -- descending steps, if Nothing narrowing is not used
  While -> -- input While statement
  NonRelational Text a -> -- input abstract state
  (NonRelational Text a, Set (NonRelational Text a, Bexp)) -- tuple of output abstract state and set of loop invariants
absWhileSemantics = absWhileSemantics' Set.empty -- tail recursive subfunction
 where
  -- evaluating a statements against a Bottom state will always result in a Bottom state
  absWhileSemantics' inv _ _ _ _ Bottom = (bottom, inv)
  -- the assignment operation is given by the abstract state's implementation
  absWhileSemantics' inv _ _ _ (Assignment x e) s = (assign (x, e) s, inv)
  -- skip does nothing, it simply returns the input state and the invariants accumulator
  absWhileSemantics' inv _ _ _ Skip s = (s, inv)
  -- for composition it first evaluates the first statement, then it evaluates the
  -- second statement using the new abstract state and the new invariants accumulator
  absWhileSemantics' inv thresholds widenDelay descendSteps (Composition e1 e2) s =
    let (x1, inv') = absWhileSemantics' inv thresholds widenDelay descendSteps e1 s
    in absWhileSemantics' inv' thresholds widenDelay descendSteps e2 x1
  -- if the else branches are evaluated separately on the states obtained if the condition is
  -- true/false, then the least upper bound of the results is returned, alongside the union
  -- of the loop invariants accumulator
  absWhileSemantics' inv thresholds widenDelay descendSteps (IfThenElse b e1 e2) s =
    let
      (thenBranch, invThen) = absWhileSemantics' inv thresholds widenDelay descendSteps e1 (absBexpSemantics b s)
      (elseBranch, invElse) = absWhileSemantics' inv thresholds widenDelay descendSteps e2 (absBexpSemantics (BexpUnary Not b) s)
    in
      (thenBranch `lub` elseBranch, invThen `Set.union` invElse)
  -- while loops are evaluated by looking for the smallest fixpoint of the loop
  absWhileSemantics' inv thresholds widenDelay descendSteps (WhileDo b e) s =
    let
      -- first define the loop's functional as the least upper bound of the input state and the concatenation
      -- of the body's semantics with the guard's semantics, evaluated over the current state x bound by the
      -- functional
      f x = s `lub` (fst . absWhileSemantics' inv thresholds widenDelay descendSteps e . absBexpSemantics b) x

      -- then find a postfix point of the functional through Kleeni iterates, with optional widening
      -- if widening is not applied, an abstract fixpoint is found instead of a postfixpoint
      postFix = iterateW f Bottom widenDelay

      -- find the best least fixpoint approximation by narrowing the postfixpoint for a fixed amount of
      -- times in order to ensure convergence
      lfp = iterateN f postFix descendSteps
    in
      -- lastly filter the lfp state with the negated guard of the loop, and keep track of the
      -- lfp as it is the loop's invariant
      (absBexpSemantics (BexpUnary Not b) lfp, Set.insert (lfp, b) inv)
   where
    -- Kleeni iterations with optional widening
    iterateW g x (Just 0) =
      let x' = widening thresholds x (g x)
      in if x' == x then x else iterateW g x' (Just 0)
    iterateW g x k =
      let x' = g x
      in if x' == x then x else iterateW g x' (decrMaybe k)

    -- Kleeni iterations with narrowing
    iterateN _ x Nothing = x
    iterateN _ x (Just 0) = x
    iterateN g x n =
      let x' = x `narrowing` g x
      in if x' == x then x else iterateN g x' (decrMaybe n)