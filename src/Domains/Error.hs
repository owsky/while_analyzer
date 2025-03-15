module Domains.Error where

import Abstract.Domain (AbstractDomain (..))
import Abstract.State (NonRelational (NonRelational))
import Abstract.Value (AbstractValue (..))
import Ast.AexpAst (AexpBinaryOp, AexpUnaryOp)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import ExtendedInt (ExtendedInt)

-- | ADT for error types, currently only division by zero is supported
data ErrorType
  = DivisionByZero
  -- enum and bounded instances are only needed for autogeneration of top element
  deriving (Enum, Bounded, Ord, Eq)

instance Show ErrorType where
  show :: ErrorType -> String
  show DivisionByZero = "possible division by zero"

-- | ADT for the Error Domain
newtype Error = Error (Set ErrorType) deriving (Show, Ord, Eq)

-- | Pretty prints the set of errors
showErrors :: Set ErrorType -> String
showErrors e = intercalate ", " (map show $ Set.toList e)

-- | Making Error an instance of AbstractDomain, so it can be used in a
-- | product domain
instance AbstractDomain Error where
  -- \| Partial order relation for errors, simply set inclusion
  leq :: Error -> Error -> Bool
  leq (Error s1) (Error s2) = s1 `Set.isSubsetOf` s2

  -- \| Bottom element of the lattice, no errors detected
  bottom :: Error
  bottom = Error Set.empty

  -- \| Top element of the lattice, all errors detected
  top :: Error
  top = Error $ Set.fromList [minBound .. maxBound]

  -- \| Least upper bound operator, given two alarms the error sets are merged
  lub :: Error -> Error -> Error
  lub (Error s1) (Error s2) = Error $ s1 `Set.union` s2

  -- \| Greatest lower bound operator, given two alarms the error sets are merged
  -- \| if only one element has error alarms, its set is propagated
  glb :: Error -> Error -> Error
  glb (Error s1) (Error s2) = Error $ s1 `Set.intersection` s2

  -- \| Widening operator for errors, the error sets are
  -- \| propagated
  widening :: Set ExtendedInt -> Error -> Error -> Error
  widening _ (Error s1) (Error s2) = Error $ s1 `Set.union` s2

  -- \| Narrowing operator for errors, the error sets are propagated
  narrowing :: Error -> Error -> Error
  narrowing (Error s1) (Error s2) = Error $ s1 `Set.union` s2

-- | Making Error an instance of AbstractValue, so it can be used in a
-- | product domain
instance AbstractValue Error where
  -- \| Abstraction function for errors, a set of constants cannot give
  -- \| rise to runtime errors
  alpha :: Maybe ([Int] -> Error)
  alpha = Just $ const bottom

  -- \| Concretization function for errors, this doesn't carry any
  -- \| particular meaning
  gamma :: Error -> [Int]
  gamma = const []

  -- \| Constant abstraction for errors, constants
  -- \| do not give rise to errors
  absConst :: Int -> Error
  absConst _ = bottom

  -- \| Unary operator for errors, the errors are propagated
  absUnary :: AexpUnaryOp -> Error -> Error
  absUnary _ = id

  -- \| Binary operator for errors, the errors are propagated
  absBinary :: AexpBinaryOp -> Error -> Error -> Error
  absBinary _ (Error s1) (Error s2) = Error $ s1 `Set.union` s2

  -- \| Backwards binary operator for errors, the errors from the result
  -- \| are ignored, as errors shouldn't be backpropagated during constraint refinements
  backAbsUnary :: AexpUnaryOp -> (Error, Error) -> Error
  backAbsUnary _ (Error s1, Error _) = Error s1

  -- \| Backwards binary operator for errors, the errors from the result
  -- \| are ignored, as errors shouldn't be backpropagated during constraint refinements
  backAbsBinary :: AexpBinaryOp -> (Error, Error, Error) -> (Error, Error)
  backAbsBinary _ (Error s1, Error s2, Error _) = (Error s1, Error s2)

  -- errors carry no numerical meaning, thus these terms are just top so performing
  -- a glb with them during backpropagation doesn't change the error values

  negative :: Error
  negative = top

  nonPositive :: Error
  nonPositive = top

  positive :: Error
  positive = top

  nonNegative :: Error
  nonNegative = top

  includesZero :: Error -> Bool
  includesZero = const False

-- | Type of abstract states domain for the error value
type ErrorState = NonRelational Int Error

-- | Helper function which creates a new error abstract state from a list of program
-- | points occurring in the program
createErrorState :: [Int] -> ErrorState
createErrorState points = NonRelational False $ Map.fromList (map (,bottom) points)