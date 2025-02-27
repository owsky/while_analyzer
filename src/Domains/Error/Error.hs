module Domains.Error.Error (Error (..), ErrorType (..), singleton, addError, showErrors) where

import Abstract.Domain (AbstractDomain (..))
import Abstract.Value (AbstractValue (..))
import Ast.AexpAst (AexpBinaryOp, AexpUnaryOp)
import Data.List (intercalate)
import Data.Set (Set)
import Data.Set qualified as Set
import ExtendedInt (ExtendedInt)

-- | ADT for error types, currently only division by zero is supported
data ErrorType = DivisionByZero deriving (Enum, Bounded, Ord, Eq)

instance Show ErrorType where
  show :: ErrorType -> String
  show DivisionByZero = "possible division by zero"

-- | Set of all error types
allErrors :: Set ErrorType
allErrors = Set.fromList [minBound .. maxBound]

-- | ADT for the Error Domain
data Error
  = NoError
  | Alarm (Set ErrorType)
  deriving (Show, Ord, Eq)

-- | Creates an alarm from a single error
singleton :: ErrorType -> Error
singleton e = Alarm $ Set.singleton e

addError :: ErrorType -> Error -> Error
addError e NoError = singleton e
addError e (Alarm s) = Alarm $ Set.insert e s

-- | Pretty prints the set of errors
showErrors :: Set ErrorType -> String
showErrors e = intercalate ", " (map show $ Set.toList e)

-- | Making Error an instance of AbstractDomain, so it can be used in a
-- | product domain
instance AbstractDomain Error where
  -- \| Partial order relation for errors
  leq :: Error -> Error -> Bool
  leq (Alarm _) _ = True
  leq _ NoError = True
  leq _ _ = False

  -- \| Bottom element of the lattice, alarm with all supported errors
  bottom :: Error
  bottom = Alarm allErrors

  -- \| Top element of the lattice, no error detected
  top :: Error
  top = NoError

  -- \| Least upper bound operator, given two alarms the error sets are merged
  lub :: Error -> Error -> Error
  lub NoError _ = NoError
  lub _ NoError = NoError
  lub (Alarm e1) (Alarm e2) = Alarm $ e1 `Set.union` e2

  -- \| Greatest lower bound operator, given two alarms the error sets are merged
  -- \| if only one element has error alarms, its set is propagated
  glb :: Error -> Error -> Error
  glb (Alarm e1) (Alarm e2) = Alarm $ e1 `Set.union` e2
  glb (Alarm e1) _ = Alarm e1
  glb _ (Alarm e2) = Alarm e2
  glb _ _ = NoError

  -- \| Widening operator for errors, the error sets are
  -- \| propagated
  widening :: Set ExtendedInt -> Error -> Error -> Error
  widening _ (Alarm e1) (Alarm e2) = Alarm $ e1 `Set.union` e2
  widening _ (Alarm e1) _ = Alarm e1
  widening _ _ (Alarm e2) = Alarm e2
  widening _ _ _ = NoError

  -- \| Narrowing operator for errors, the error sets are propagated
  narrowing :: Error -> Error -> Error
  narrowing (Alarm e1) (Alarm e2) = Alarm $ e1 `Set.union` e2
  narrowing (Alarm e1) _ = Alarm e1
  narrowing _ (Alarm e2) = Alarm e2
  narrowing _ _ = NoError

-- | Making Error an instance of AbstractValue, so it can be used in a
-- | product domain
instance AbstractValue Error where
  -- \| Abstraction function for errors
  alpha :: Maybe ([Int] -> Error)
  alpha = undefined

  -- \| Concretization function for errors
  gamma :: Error -> [Int]
  gamma = undefined

  -- \| Constant abstraction for errors, constants
  -- \| do not give rise to errors
  absConst :: Int -> Error
  absConst _ = NoError

  -- \| Unary operator for errors, the errors are propagated
  absUnary :: AexpUnaryOp -> Error -> Error
  absUnary _ = id

  -- \| Binary operator for errors, the errors are propagated
  absBinary :: AexpBinaryOp -> Error -> Error -> Error
  absBinary _ (Alarm e1) (Alarm e2) = Alarm $ e1 `Set.union` e2
  absBinary _ (Alarm e1) _ = Alarm e1
  absBinary _ _ (Alarm e2) = Alarm e2
  absBinary _ _ _ = NoError

  -- \| Backwards unary operator for errors, the errors are propagated
  -- \| from the result to the operand
  backAbsUnary :: AexpUnaryOp -> (Error, Error) -> Error
  backAbsUnary _ (Alarm e1, Alarm e2) = Alarm $ e1 `Set.union` e2
  backAbsUnary _ (_, Alarm e2) = Alarm e2
  backAbsUnary _ (Alarm e1, _) = Alarm e1
  backAbsUnary _ _ = NoError

  -- \| Backwards binary operator for errors, the errors from the result
  -- \| are propagated backwards to the operands
  backAbsBinary :: AexpBinaryOp -> (Error, Error, Error) -> (Error, Error)
  backAbsBinary _ (Alarm e1, Alarm e2, Alarm e3) = (Alarm $ e3 `Set.union` e1, Alarm $ e3 `Set.union` e2)
  backAbsBinary _ (NoError, Alarm e2, Alarm e3) = (Alarm e3, Alarm $ e3 `Set.union` e2)
  backAbsBinary _ (Alarm e1, NoError, Alarm e3) = (Alarm $ e3 `Set.union` e1, Alarm e3)
  backAbsBinary _ (Alarm e1, Alarm e2, NoError) = (Alarm e1, Alarm e2)
  backAbsBinary _ (NoError, Alarm e2, NoError) = (NoError, Alarm e2)
  backAbsBinary _ (Alarm e1, NoError, NoError) = (Alarm e1, NoError)
  backAbsBinary _ (NoError, NoError, Alarm e3) = (Alarm e3, Alarm e3)
  backAbsBinary _ (NoError, NoError, NoError) = (NoError, NoError)

  negative :: Error
  negative = NoError

  nonPositive :: Error
  nonPositive = NoError

  positive :: Error
  positive = NoError

  nonNegative :: Error
  nonNegative = NoError

  includesZero :: Error -> Bool
  includesZero = const False