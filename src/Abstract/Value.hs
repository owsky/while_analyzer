module Abstract.Value (AbstractValue (..)) where

import Abstract.Domain (AbstractDomain)
import Ast.AexpAst (AexpBinaryOp, AexpUnaryOp)

-- | Type class for the abstract domain of values
class (AbstractDomain a) => AbstractValue a where
  -- | Abstraction function, possibly undefined
  alpha :: Maybe ([Int] -> a)

  -- | Concretization function
  gamma :: a -> [Int]

  -- | Abstraction of integer constants
  absConst :: Int -> a

  -- | Abstraction of arithmetic unary operators
  absUnary :: AexpUnaryOp -> a -> a

  -- | Abstraction of arithmetic binary operators
  absBinary :: AexpBinaryOp -> a -> a -> a

  -- | Operators and values required for backpropagating arithmetic
  -- | results during condition checks

  -- | Abstraction of backwards arithmetic unary operators
  backAbsUnary :: AexpUnaryOp -> (a, a) -> a

  -- | Abstraction of backwards arithmetic binary operators
  backAbsBinary :: AexpBinaryOp -> (a, a, a) -> (a, a)

  -- | Abstract value for negative values (< 0)
  negative :: a

  -- | Abstract value for non positive values (<= 0)
  nonPositive :: a

  -- | Abstract value for positive values (> 0)
  positive :: a

  -- | Abstract value for non negative values (>= 0)
  nonNegative :: a