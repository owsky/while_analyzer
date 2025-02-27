module Abstract.Domain (AbstractDomain (..)) where

import Data.Set (Set)
import ExtendedInt (ExtendedInt)

-- | Type class for abstract domains, parameterized on
-- | complete lattices
class AbstractDomain a where
  -- | Partial order relation between abstract values
  leq :: a -> a -> Bool

  -- | Bottom element of the lattice
  bottom :: a

  -- | Top element of the lattice
  top :: a

  -- | Abstract least upper bound operator
  lub :: a -> a -> a

  -- | Abstract greatest lower bound operator
  glb :: a -> a -> a

  -- | Widening operator defined as widening with thresholds,
  -- | so it requires a set of numerical constants to use as thresholds
  -- | It defaults to the abstract least upper bound, if not defined by
  -- | the instance
  widening :: Set ExtendedInt -> a -> a -> a
  widening _ = lub

  -- | Narrowing operator. It defaults to the abstract greatest lower
  -- | bound, if not defined by the instance
  narrowing :: a -> a -> a
  narrowing = glb

  -- | Checks if a value is equal to bottom through reflexivity of the
  -- | partial order
  isBottom :: a -> Bool
  isBottom x = x `leq` bottom