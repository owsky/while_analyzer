module Domains.Error.Product (ProductError (..), splitDomain) where

import Abstract.Aexp (absAexpSemantics)
import Abstract.Domain (AbstractDomain (..))
import Abstract.State (AbstractState (..), NonRelational (..))
import Abstract.Value (AbstractValue (..))
import Ast.AexpAst (Aexp, AexpBinaryOp (Div), AexpUnaryOp)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Domains.Error.Error (Error (..), ErrorType (DivisionByZero), showErrors)
import ExtendedInt (ExtendedInt)
import State (update)

-- | ADT for an abstract product domain between a generic product domain a
-- | and the error domain
data ProductError a = (AbstractDomain a) => ProductError a Error

instance (Show a) => Show (ProductError a) where
  show :: ProductError a -> String
  show (ProductError a (Error s)) = "(" <> show a <> "," <> showErrors s <> " )"

-- | Eq instance of ProductError, required by sets
instance (AbstractDomain a) => Eq (ProductError a) where
  (==) :: ProductError a -> ProductError a -> Bool
  (==) x y = x `leq` y && y `leq` x
  (/=) :: ProductError a -> ProductError a -> Bool
  (/=) x y = not $ x == y

-- | Ord instance of ProductError, doesn't carry any particular meaning
-- | Required by sets
instance (AbstractDomain a) => Ord (ProductError a) where
  compare :: ProductError a -> ProductError a -> Ordering
  compare x y = case x `leq` y of
    True -> LT
    False -> GT

-- | Making ProductError a an instance of AbstractDomain, so it can be used
-- | in-place of a
instance (AbstractDomain a) => AbstractDomain (ProductError a) where
  -- \| Partial order relation for the product domain
  leq :: ProductError a -> ProductError a -> Bool
  leq (ProductError a1 a2) (ProductError b1 b2) = (a1 `leq` b1) && (a2 `leq` b2)

  -- \| Bottom element of the product domain
  bottom :: ProductError a
  bottom = ProductError bottom bottom

  -- \| Top element of the product domain
  top :: ProductError a
  top = ProductError top top

  -- \| Least upper bound relation of the product domain
  lub :: ProductError a -> ProductError a -> ProductError a
  lub (ProductError a1 a2) (ProductError b1 b2) = ProductError (a1 `lub` b1) (a2 `lub` b2)

  -- \| Greatest lower bound relation of the product domain
  glb :: ProductError a -> ProductError a -> ProductError a
  glb (ProductError a1 a2) (ProductError b1 b2) = ProductError (a1 `glb` b1) (a2 `glb` b2)

  -- \| Widening operator of the product domain
  widening :: Set ExtendedInt -> ProductError a -> ProductError a -> ProductError a
  widening thresholds (ProductError a1 a2) (ProductError b1 b2) = ProductError (widening thresholds a1 b1) (widening thresholds a2 b2)

  -- \| Narrowing operator of the product domain
  narrowing :: ProductError a -> ProductError a -> ProductError a
  narrowing (ProductError a1 a2) (ProductError b1 b2) = ProductError (a1 `narrowing` b1) (a2 `narrowing` b2)

-- | Making ProductError a an instance of AbstractValue
instance (AbstractValue a) => AbstractValue (ProductError a) where
  -- \| Abstraction function of the ProductError domain, constants never give rise to
  -- \| runtime errors, so only a's alpha is used
  alpha :: Maybe ([Int] -> ProductError a)
  alpha = case (alpha @a) of
    (Just alphaA) -> Just $ \inp -> ProductError (alphaA inp) top
    _ -> Nothing

  -- \| Concretization function of the ProductError domain, errors do not
  -- \| concretize to the integer domain so only a's gamma is used
  gamma :: ProductError a -> [Int]
  gamma (ProductError vals _) = gamma vals

  -- \| Constant abstraction, uses the underlying methods
  absConst :: Int -> ProductError a
  absConst k = ProductError (absConst k) (absConst k)

  -- \| Unary operator abstraction, uses the underlying methods
  absUnary :: AexpUnaryOp -> ProductError a -> ProductError a
  absUnary op (ProductError a err) = ProductError (absUnary op a) (absUnary op err)

  -- \| Binary operator abstraction, uses the underlying methods, with the exception of integer division
  -- \| where the behavior is changed so that, if the denominator includes the zero, a division by zero
  -- \| alarm is raised
  absBinary :: AexpBinaryOp -> ProductError a -> ProductError a -> ProductError a
  absBinary op (ProductError a1 a2) (ProductError b1 b2) = case op of
    Div
      | includesZero b1 ->
          let (Error s) = absBinary op a2 b2
              s' = Error $ Set.insert DivisionByZero s
          in ProductError (absBinary op a1 b1) s'
    _ -> ProductError (absBinary op a1 b1) (absBinary op a2 b2)

  -- \| Backawrds unary operator abstrction, uses the underlying methods
  backAbsUnary :: AexpUnaryOp -> (ProductError a, ProductError a) -> ProductError a
  backAbsUnary op (ProductError a1 a2, ProductError b1 b2) = ProductError (backAbsUnary op (a1, b1)) (backAbsUnary op (a2, b2))

  -- \| Backawrds binary operators abstrction, uses the underlying methods
  backAbsBinary ::
    AexpBinaryOp -> (ProductError a, ProductError a, ProductError a) -> (ProductError a, ProductError a)
  backAbsBinary op (ProductError a1 a2, ProductError b1 b2, ProductError c1 c2) =
    let (backA1, backA2) = backAbsBinary op (a1, b1, c1)
        (backB1, backB2) = backAbsBinary op (a2, b2, c2)
    in (ProductError backA1 backB1, ProductError backA2 backB2)

  negative :: ProductError a
  negative = ProductError negative negative

  nonPositive :: ProductError a
  nonPositive = ProductError nonPositive nonPositive

  positive :: ProductError a
  positive = ProductError positive positive

  nonNegative :: ProductError a
  nonNegative = ProductError nonNegative nonNegative

  includesZero :: (AbstractValue a) => ProductError a -> Bool
  includesZero (ProductError a _) = includesZero a

-- | Making the non relational state of the ProductError domain, an abstract state
instance (AbstractValue a, Show a) => AbstractState (NonRelational Text (ProductError a)) where
  assign :: (Text, Aexp) -> NonRelational Text (ProductError a) -> NonRelational Text (ProductError a)
  assign _ Bottom = Bottom
  assign (x, aexp) s = case absAexpSemantics aexp s of
    e -> update s x e

-- | Helper function which splits a ProudctError state into two separate states, each containing the same
-- | variables but one with the abstract values associated to them, the other the runtime errors detected
splitDomain ::
  (Show a, Ord a) => NonRelational Text (ProductError a) -> (NonRelational Text a, NonRelational Text Error)
splitDomain Bottom = (Bottom, Bottom)
splitDomain (NonRelational _ x) =
  foldr
    (\(varName, ProductError a err) (aS, errS) -> (update aS varName a, update errS varName err))
    (NonRelational True Map.empty, NonRelational False Map.empty)
    (Map.assocs x)