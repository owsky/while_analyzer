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
absWhileSemantics thrs wd ds w inputState a = absWhileSemantics' w inputState a Map.empty
 where
  -- evaluating a statements against a Bottom state will always result in a Bottom state
  absWhileSemantics' _ Bottom alarms invs = (bottom, alarms, invs)
  -- the assignment operation is given by the abstract state's implementation
  absWhileSemantics' (Assignment point x e) s alarms invs =
    let (val, err) = absAexpSemantics e s
    in (assign x val s, updateAlarms alarms point err, invs)
  -- skip does nothing, it simply returns the input state and the invariants accumulator
  absWhileSemantics' (Skip _) s alarms invs = (s, alarms, invs)
  -- for composition it first evaluates the first statement, then it evaluates the
  -- second statement using the new abstract state and the new invariants accumulator
  absWhileSemantics' (Composition e1 e2) s alarms invs =
    let (x1, alarms', inv') = absWhileSemantics' e1 s alarms invs
    in absWhileSemantics' e2 x1 alarms' inv'
  -- if then else branches are evaluated separately on the states obtained if the condition is
  -- true/false, then the least upper bound of the results is returned, alongside the union
  -- of the loop invariants accumulator
  absWhileSemantics' (IfThenElse point b e1 e2) s alarms invs =
    let
      (thenBranch, thenAlarms, thenInvs) = composeWhileBexp point e1 b s alarms invs
      (elseBranch, elseAlarms, elseInvs) = composeWhileBexp point e2 (BexpUnary Not b) s alarms invs
    in
      (thenBranch `lub` elseBranch, mergeAlarms thenAlarms elseAlarms, thenInvs `Map.union` elseInvs)
  -- while loops are evaluated by looking for the smallest fixpoint of the loop
  absWhileSemantics' (WhileDo point b e) s alarms invs =
    let
      -- first define the loop's functional as the least upper bound of the input state and the concatenation
      -- of the body's semantics with the guard's semantics, evaluated over the current state x bound by the
      -- functional
      f (x, alarms', inv') =
        let (newState, newAlarms, newInvs) = composeWhileBexp point e b x alarms' inv'
        in (s `lub` newState, newAlarms, newInvs)

      -- then find a postfix point of the functional through Kleeni iterates, with optional widening
      -- if widening is not applied, an abstract fixpoint is found instead of a postfixpoint
      postFix = iterateW f (Bottom, alarms, invs) wd

      -- find the best least fixpoint approximation by narrowing the postfixpoint for a fixed amount of
      -- times in order to ensure convergence
      (finalS, finalErr, finalInv) = iterateN f postFix ds

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
          wideX = widening thrs x x'
      in if wideX == x then (x, alarmsAcc', invAcc') else iterateW g (wideX, alarmsAcc', invAcc') (Just 0)
    iterateW g (x, alarmsAcc, invAcc) widenDelay =
      let (x', alarmsAcc', invAcc') = g (x, alarmsAcc, invAcc)
      in if x' == x then (x, alarmsAcc', invAcc') else iterateW g (x', alarmsAcc', invAcc') (decrMaybe widenDelay)

    -- Kleeni iterations with narrowing
    -- if descending steps is given, the glb is used for narrowing a fixed number of times
    -- otherwise the narrowing operator is used until convergence
    iterateN _ x (Just 0) = x
    iterateN g (x, alarmsAcc, invAcc) descendSteps =
      let narrowOp = if isNothing descendSteps then narrowing else glb
          (x', alarmsAcc', invAcc') = g (x, alarmsAcc, invAcc)
          narrowX = x `narrowOp` x'
      in if x' == x then (x, alarmsAcc', invAcc') else iterateN g (narrowX, alarmsAcc', invAcc') (decrMaybe descendSteps)

  -- \| Composition of while and bexp semantics such that While . Bexp -> While[w](Bexp[b]s)
  composeWhileBexp point statement b s alarms invs =
    let (bexpState, bexpAlarms) = absBexpSemantics b point s alarms
    in absWhileSemantics' statement bexpState bexpAlarms invs
