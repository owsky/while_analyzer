module Abstract.While (absWhileSemantics, Invariants) where

import Abstract.Bexp (absBexpSemantics)
import Abstract.Domain (AbstractDomain (..))
import Abstract.State (AbstractState (..), NonRelational (..))
import Abstract.Value (AbstractValue)
import Ast.BexpAst (Bexp (..), BoolUnaryOp (..))
import Ast.WhileAst (While (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Text (Text)
import ExtendedInt (ExtendedInt)
import Utils (decrMaybe)

type Invariants a = Map Int (NonRelational Text a)

-- | Abstract evaluation of While programs, defined by induction on their
-- | denotational semantics
absWhileSemantics ::
  (AbstractValue a, AbstractState (NonRelational Text a), Show a, Eq a, Ord a) =>
  Set ExtendedInt -> -- numeric thresholds to use during widening
  Maybe Int -> -- widening delay, if Nothing widening is not used
  Maybe Int -> -- descending steps, if Nothing narrowing is not used
  While -> -- input While statement
  NonRelational Text a -> -- input abstract state
  (NonRelational Text a, Invariants a) -- tuple of output abstract state and set of loop invariants
absWhileSemantics = absWhileSemantics' Map.empty -- tail recursive subfunction
 where
  -- evaluating a statements against a Bottom state will always result in a Bottom state
  absWhileSemantics' inv _ _ _ _ Bottom = (bottom, inv)
  -- the assignment operation is given by the abstract state's implementation
  absWhileSemantics' inv _ _ _ (Assignment point x e) s = (assign (x, e) s, inv)
  -- skip does nothing, it simply returns the input state and the invariants accumulator
  absWhileSemantics' inv _ _ _ (Skip _) s = (s, inv)
  -- for composition it first evaluates the first statement, then it evaluates the
  -- second statement using the new abstract state and the new invariants accumulator
  absWhileSemantics' inv thresholds widenDelay descendSteps (Composition e1 e2) s =
    let (x1, inv') = absWhileSemantics' inv thresholds widenDelay descendSteps e1 s
    in absWhileSemantics' inv' thresholds widenDelay descendSteps e2 x1
  -- if the else branches are evaluated separately on the states obtained if the condition is
  -- true/false, then the least upper bound of the results is returned, alongside the union
  -- of the loop invariants accumulator
  absWhileSemantics' inv thresholds widenDelay descendSteps (IfThenElse point b e1 e2) s =
    let
      (thenBranch, invThen) = absWhileSemantics' inv thresholds widenDelay descendSteps e1 (absBexpSemantics b s)
      (elseBranch, invElse) = absWhileSemantics' inv thresholds widenDelay descendSteps e2 (absBexpSemantics (BexpUnary Not b) s)
    in
      (thenBranch `lub` elseBranch, invThen `Map.union` invElse)
  -- while loops are evaluated by looking for the smallest fixpoint of the loop
  absWhileSemantics' inv thresholds widenDelay descendSteps (WhileDo point b e) s =
    let
      -- first define the loop's functional as the least upper bound of the input state and the concatenation
      -- of the body's semantics with the guard's semantics, evaluated over the current state x bound by the
      -- functional
      f (x, _) =
        let (newState, inv') = (absWhileSemantics' inv thresholds widenDelay descendSteps e . absBexpSemantics b) x
        in (s `lub` newState, inv')

      -- then find a postfix point of the functional through Kleeni iterates, with optional widening
      -- if widening is not applied, an abstract fixpoint is found instead of a postfixpoint
      postFix = iterateW f (Bottom, inv) widenDelay

      -- find the best least fixpoint approximation by narrowing the postfixpoint for a fixed amount of
      -- times in order to ensure convergence
      (finalState, finalInv) = iterateN f postFix descendSteps
    in
      -- lastly filter the lfp state with the negated guard of the loop, and keep track of the
      -- lfp as it is the loop's invariant
      (absBexpSemantics (BexpUnary Not b) finalState, Map.insert point finalState finalInv)
   where
    -- Kleeni iterations with optional widening
    iterateW g (x, invAcc) (Just 0) =
      let (x', invAcc') = g (x, invAcc)
          wideX = widening thresholds x x'
      in if wideX == x then (x, invAcc) else iterateW g (wideX, invAcc') (Just 0)
    iterateW g (x, invAcc) k =
      let (x', invAcc') = g (x, invAcc)
      in if x' == x then (x, invAcc) else iterateW g (x', invAcc') (decrMaybe k)

    -- Kleeni iterations with narrowing
    -- if descending steps is given, the glb is used for narrowing a fixed number of times
    -- otherwise the narrowing operator is used until convergence
    iterateN _ x (Just 0) = x
    iterateN g (x, invAcc) descendSteps' =
      let narrowOp = if isNothing descendSteps' then narrowing else glb
          (x', invAcc') = g (x, invAcc)
          narrowX = x `narrowOp` x'
      in if x' == x then (x, invAcc) else iterateN g (narrowX, invAcc') (decrMaybe descendSteps')