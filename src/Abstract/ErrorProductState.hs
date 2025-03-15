module Abstract.ErrorProductState where

import Abstract.Domain (AbstractDomain (..))
import Abstract.Semantics.Aexp (absAexpSemantics)
import Abstract.State (AbstractState (..), NonRelational (..))
import Abstract.Value (AbstractValue)
import Ast.AexpAst (Aexp)
import Data.Set (Set)
import Data.Text (Text)
import Domains.Error (ErrorState)
import ExtendedInt (ExtendedInt)
import State (State (update))

-- | ADT for a direct product domain between a generic abstract domain a and the error domain,
-- | used during the analysis to keep track of the abstract states for values and errors
data ErrorProductState a
  = ErrorProductState
  { valueState :: NonRelational Text a
  , errorState :: ErrorState
  }
  deriving (Eq, Show)

-- | Making product domain an instance of Abstract Domain so it can be used as an abstract state.
-- | The methods are implemented pointwise
instance (AbstractValue a, Show a, Ord a) => AbstractDomain (ErrorProductState a) where
  leq :: (AbstractValue a) => ErrorProductState a -> ErrorProductState a -> Bool
  leq (ErrorProductState vs1 es1) (ErrorProductState vs2 es2) = vs1 `leq` vs2 && es1 `leq` es2

  bottom :: (AbstractValue a) => ErrorProductState a
  bottom = ErrorProductState bottom bottom

  top :: (AbstractValue a) => ErrorProductState a
  top = ErrorProductState top top

  lub :: (AbstractValue a) => ErrorProductState a -> ErrorProductState a -> ErrorProductState a
  lub (ErrorProductState vs1 es1) (ErrorProductState vs2 es2) = ErrorProductState (vs1 `lub` vs2) (es1 `lub` es2)

  glb :: (AbstractValue a) => ErrorProductState a -> ErrorProductState a -> ErrorProductState a
  glb (ErrorProductState vs1 es1) (ErrorProductState vs2 es2) = ErrorProductState (vs1 `glb` vs2) (es1 `glb` es2)

  widening :: (AbstractValue a) => Set ExtendedInt -> ErrorProductState a -> ErrorProductState a -> ErrorProductState a
  widening thrs (ErrorProductState vs1 es1) (ErrorProductState vs2 es2) = ErrorProductState (widening thrs vs1 vs2) (widening thrs es1 es2)

  narrowing :: (AbstractValue a) => ErrorProductState a -> ErrorProductState a -> ErrorProductState a
  narrowing (ErrorProductState vs1 es1) (ErrorProductState vs2 es2) = ErrorProductState (narrowing vs1 vs2) (narrowing es1 es2)

-- | Making product state an instance of Abstract State
instance (AbstractValue a, Show a, Ord a) => AbstractState (ErrorProductState a) (Text, Int) where
  -- \| The assignment transfer function is implemented as the evaluation of the expression in the given state,
  -- \| which returns both the abstract value it evaluates to and the eventual errors occurring during evaluation,
  -- \| then the respective abstract states for values and errors are updated individually
  assign :: (AbstractValue a, Show a, Ord a) => ((Text, Int), Aexp) -> ErrorProductState a -> ErrorProductState a
  assign ((varName, point), e) state@(ErrorProductState vs es) =
    let (val, err) = absAexpSemantics e state
    in ErrorProductState (update vs varName val) (update es point err)
