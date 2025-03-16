module Abstract.Semantics.While (absWhileSemantics, Invariants) where

import Abstract.Domain (AbstractDomain (..))
import Abstract.Semantics.Aexp (absAexpSemantics)
import Abstract.Semantics.Bexp (absBexpSemantics)
import Abstract.State (AbstractState (..), NonRelational (..))
import Abstract.Value (AbstractValue)
import Alarms (Alarms, mergeAlarms, updateAlarms)
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

-- | Abstract evaluation of While programs, defined by induction on their denotational semantics
absWhileSemantics ::
  ( AbstractValue a
  , Show a
  , Eq a
  , Ord a
  , AbstractState (NonRelational Text a) Text a
  ) =>
  Set ExtendedInt -> -- numeric thresholds to use during widening
  Maybe Int -> -- widening delay, if Nothing widening is not used
  Maybe Int -> -- descending steps, if Nothing narrowing is not used
  While -> -- input While statement
  NonRelational Text a -> -- input abstract state
  Alarms -> -- alarms for each program point
  (NonRelational Text a, Alarms, Invariants a) -- tuple of output abstract state, alarms and map of loop invariants
absWhileSemantics = absWhileSemantics' Map.empty -- tail recursive subfunction
 where
  -- evaluating a statements against a Bottom state will always result in a Bottom state
  absWhileSemantics' inv _ _ _ _ Bottom alarms = (bottom, alarms, inv)
  -- the assignment operation is given by the abstract state's implementation
  absWhileSemantics' inv _ _ _ (Assignment point x e) s alarms =
    let (val, err) = absAexpSemantics e s
    in (assign x val s, updateAlarms alarms point err, inv)
  -- skip does nothing, it simply returns the input state and the invariants accumulator
  absWhileSemantics' inv _ _ _ (Skip _) s alarms = (s, alarms, inv)
  -- for composition it first evaluates the first statement, then it evaluates the
  -- second statement using the new abstract state and the new invariants accumulator
  absWhileSemantics' inv thresholds widenDelay descendSteps (Composition e1 e2) s alarms =
    let (x1, alarms', inv') = absWhileSemantics' inv thresholds widenDelay descendSteps e1 s alarms
    in absWhileSemantics' inv' thresholds widenDelay descendSteps e2 x1 alarms'
  -- if then else branches are evaluated separately on the states obtained if the condition is
  -- true/false, then the least upper bound of the results is returned, alongside the union
  -- of the loop invariants accumulator
  absWhileSemantics' inv thresholds widenDelay descendSteps (IfThenElse point b e1 e2) s alarms =
    let
      (trueGuardS, trueGuardAlarms) = absBexpSemantics b point s alarms
      (falseGuardS, falseGuardAlarms) = absBexpSemantics (BexpUnary Not b) point s alarms
      guardAlarms = mergeAlarms trueGuardAlarms falseGuardAlarms
      (thenBranch, thenAlarms, thenInvs) = absWhileSemantics' inv thresholds widenDelay descendSteps e1 trueGuardS guardAlarms
      (elseBranch, elseAlarms, elseInvs) = absWhileSemantics' inv thresholds widenDelay descendSteps e2 falseGuardS guardAlarms
    in
      (thenBranch `lub` elseBranch, mergeAlarms thenAlarms elseAlarms, thenInvs `Map.union` elseInvs)
  -- while loops are evaluated by looking for the smallest fixpoint of the loop
  absWhileSemantics' inv thresholds widenDelay descendSteps (WhileDo point b e) s alarms =
    let
      -- first define the loop's functional as the least upper bound of the input state and the concatenation
      -- of the body's semantics with the guard's semantics, evaluated over the current state x bound by the
      -- functional
      f (x, alarms', inv') =
        let
          (guardS, guardAlarms) = absBexpSemantics b point x alarms'
          (newState, alarms'', inv'') = absWhileSemantics' inv' thresholds widenDelay descendSteps e guardS alarms'
          iterationAlarms = mergeAlarms alarms'' guardAlarms
        in
          (s `lub` newState, mergeAlarms alarms' iterationAlarms, inv'')

      -- then find a postfix point of the functional through Kleeni iterates, with optional widening
      -- if widening is not applied, an abstract fixpoint is found instead of a postfixpoint
      postFix = iterateW f (Bottom, alarms, inv) widenDelay

      -- find the best least fixpoint approximation by narrowing the postfixpoint for a fixed amount of
      -- times in order to ensure convergence
      (finalS, finalErr, finalInv) = iterateN f postFix descendSteps

      -- filter the lfp state with the negated guard of the loop
      (notGuardS, notGuardAlarms) = absBexpSemantics (BexpUnary Not b) point finalS finalErr
    in
      -- return the output state, the alarms updated with the ones possible raised while evaluating the
      -- loop, and the new invariants map
      (notGuardS, notGuardAlarms, Map.insert point finalS finalInv)
   where
    -- Kleeni iterations with optional widening
    iterateW g (x, alarmsAcc, invAcc) (Just 0) =
      let (x', alarmsAcc', invAcc') = g (x, alarmsAcc, invAcc)
          wideX = widening thresholds x x'
      in if wideX == x then (x, alarmsAcc', invAcc') else iterateW g (wideX, alarmsAcc', invAcc') (Just 0)
    iterateW g (x, alarmsAcc, invAcc) k =
      let (x', alarmsAcc', invAcc') = g (x, alarmsAcc, invAcc)
      in if x' == x then (x, alarmsAcc', invAcc') else iterateW g (x', alarmsAcc', invAcc') (decrMaybe k)

    -- Kleeni iterations with narrowing
    -- if descending steps is given, the glb is used for narrowing a fixed number of times
    -- otherwise the narrowing operator is used until convergence
    iterateN _ x (Just 0) = x
    iterateN g (x, alarmsAcc, invAcc) descendSteps' =
      let narrowOp = if isNothing descendSteps' then narrowing else glb
          (x', alarmsAcc', invAcc') = g (x, alarmsAcc, invAcc)
          narrowX = x `narrowOp` x'
      in if x' == x then (x, alarmsAcc', invAcc') else iterateN g (narrowX, alarmsAcc', invAcc') (decrMaybe descendSteps')