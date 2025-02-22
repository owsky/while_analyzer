{-# LANGUAGE LambdaCase #-}

module Interval.Interval (Interval (..), mkInterval, IntervalState) where

import Abstract.Aexp (absAexpSemantics)
import Abstract.Domain (AbstractDomain (..))
import Abstract.State (AbstractState (..), NonRelational (..))
import Abstract.Value (AbstractValue (..))
import Ast.AexpAst (Aexp, AexpBinaryOp (..), AexpUnaryOp (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Interval.Arithmetics (divIntervals, mulIntervals, negateInterval, subIntervals, sumIntervals)
import Interval.Bounds (clampInterval)
import Interval.ExtendedInt (ExtendedInt (..))
import State (State (..))

-- | ADT for the Interval abstract domain value
data Interval
  = Empty -- represents the bottom element of the lattice
  | Interval ExtendedInt ExtendedInt
  -- total order is derived without any particular meaning, as it's only
  -- necessary for the usage of Data.Set
  deriving (Eq, Ord)

-- | Smart constructor for the Interval type, checks if an interval built
-- | with the given bounds is legal in the domain, i.e.
-- | a == b is always allowed as it represents the constant domain
-- | a /= -> a <= b if this condition is not satisfied the resulting
-- | interval will be bottom
-- | [a, b] ⊆ [m, n] where m,n are the global bounds which parameterize the
-- | abstract domain. If this condition is not true, the best correct approximation
-- | is returned instead
mkInterval :: ExtendedInt -> ExtendedInt -> Interval
mkInterval (ExtInt a) (ExtInt b)
  | a == b = Interval (ExtInt a) (ExtInt a)
mkInterval a b
  | a <= b = clampInterval $ Interval a b
  | otherwise = Empty

-- | Show Instance for the Interval type, needed to print the analysis' output
instance Show Interval where
  show :: Interval -> String
  show Empty = "$⊥_{b}^{\\text{\\#}}$"
  show (Interval NegInf PosInf) = "$⊤_{b}^{\\text{\\#}}$"
  show (Interval l r) = "[" <> show l <> ", " <> show r <> "]"

instance Num Interval where
  (+) :: Interval -> Interval -> Interval
  (+) = sumIntervals
  (-) :: Interval -> Interval -> Interval
  (-) = subIntervals
  (*) :: Interval -> Interval -> Interval
  (*) = mulIntervals
  negate :: Interval -> Interval
  negate = negateInterval
  abs :: Interval -> Interval
  abs = undefined
  signum :: Interval -> Interval
  signum = undefined
  fromInteger :: Integer -> Interval
  fromInteger k = Interval (fromIntegral k) (fromIntegral k)

instance Fractional Interval where
  (/) :: Interval -> Interval -> Interval
  (/) = divIntervals
  recip :: Interval -> Interval
  recip = undefined
  fromRational :: Rational -> Interval
  fromRational = error "While analyzer only supports the integer data type"

-- | AbstractDomain instance of the Interval type
instance AbstractDomain Interval where
  -- \| Partial order for intervals
  leq :: Interval -> Interval -> Bool
  leq Empty _ = True
  leq _ Empty = False
  leq (Interval a b) (Interval c d) = a >= c && b <= d

  -- \| Bottom element of intervals
  bottom :: Interval
  bottom = Empty

  -- \| Top element of intervals, encoded as [-∞, +∞]
  top :: Interval
  top = Interval NegInf PosInf

  -- \| Least upper bound operator for intervals, bottom is absorbing
  lub :: Interval -> Interval -> Interval
  lub Empty y = y
  lub x Empty = x
  lub (Interval a b) (Interval c d) = mkInterval (min a c) (max b d)

  -- \| Greatest lower bound operator intervals, bottom is strict
  glb :: Interval -> Interval -> Interval
  glb Empty _ = Empty
  glb _ Empty = Empty
  glb (Interval a b) (Interval c d)
    | max a c <= min b d = mkInterval (max a c) (min b d)
    | otherwise = Empty

  -- \| Widening operator for intervals, implemented as widening with thresholds
  -- \| for higher accuracy, bottom is absorbing
  widening :: Set ExtendedInt -> Interval -> Interval -> Interval
  widening _ Empty y = y
  widening _ x Empty = x
  widening thresholds (Interval a b) (Interval c d) =
    -- instead of replacing the unstable bounds with -∞/+∞ I use the minimum/maximum
    -- threshold greater/smaller than the second interval's bound
    let lower = if a <= c then a else Set.findMax $ Set.filter (<= c) thresholds
        higher = if b >= d then b else Set.findMin $ Set.filter (>= d) thresholds
    in mkInterval lower higher

  -- \| Narrowing operator for intervals. Bottom is treated differently depending on
  -- \| the order of the terms in the operation.
  -- \| The operator △ is defined as: ∀ X,Y ∈ Interval. X ⊓ Y ⊑ X △ Y ⊑ X
  -- \| Thus if X is Bottom, the Bottom △ Y = Bottom as only Bottom ⊑ Bottom
  -- \| If instead Y is Bottom, then X △ Bottom = X as the output of the narrowing
  -- \| needs to be less defined than X, but Y = Bottom provides no means of refining X
  narrowing :: Interval -> Interval -> Interval
  narrowing Empty _ = Empty
  narrowing x Empty = x
  narrowing (Interval a b) (Interval c d) =
    let lower = if a == NegInf then c else a
        higher = if b == PosInf then d else b
    in mkInterval lower higher

-- | AbstractValue instance for the Interval type
instance AbstractValue Interval where
  -- \| The concretization and abstraction functions' implementations are provided
  -- \| for completeness' sake. They are not actually used by the analyzer

  -- \| Concretization function which maps Intervals to integers.
  -- \| Haskell's list comprehensions are lazily evaluated, thus infinite lists
  -- \| will not produce memory errors. Data.Set instead does not support infinite sets
  gamma :: Interval -> [Int]
  gamma Empty = []
  gamma (Interval (ExtInt a) (ExtInt b)) = [a .. b]
  gamma (Interval a b) =
    case (a, b) of
      (ExtInt a', PosInf) -> [a' ..]
      -- infinite list comprehensions need a starting point, so the list is generated
      -- from b' and then each value gets negated
      (NegInf, ExtInt b') -> map negate [b' ..]
      -- this is a tricky case, as there is no starting point, so the minimum
      -- integer which the machine can represent is used instead
      (NegInf, PosInf) -> [minBound :: Int ..]
      -- all other intervals are not valid sets, so the empty list is returned
      _ -> []

  -- \| Abstraction function which maps sets of integers extended with -∞ and +∞
  -- \| to Intervals
  alpha :: Maybe ([Int] -> Interval)
  alpha = Just $ \case
    [] -> bottom
    xs -> Interval (minimum $ map ExtInt xs) (maximum $ map ExtInt xs)

  -- \| Integer constants abstraction into Interval domain, namely:
  -- \| ∀ k ∈ ℤ. [k, k]
  absConst :: Int -> Interval
  absConst k = Interval (ExtInt k) (ExtInt k)

  -- \| Abstract unary operations for Intervals, namely Negation
  absUnary :: AexpUnaryOp -> Interval -> Interval
  absUnary Neg Empty = Empty
  absUnary Neg i = -i

  -- \| Abstract binary operations for Intervals with strict Bottom
  absBinary :: AexpBinaryOp -> Interval -> Interval -> Interval
  absBinary _ Empty _ = Empty
  absBinary _ _ Empty = Empty
  absBinary Sum i1 i2 = i1 + i2
  absBinary Sub i1 i2 = i1 - i2
  absBinary Mul i1 i2 = i1 * i2
  absBinary Div i1 i2 = i1 / i2

  -- \| Abstract values and abstract operations used for backpropagating arithmetic expressions results

  -- \| Backwards unary operations for Intervals, namely Negation.
  backAbsUnary :: AexpUnaryOp -> (Interval, Interval) -> Interval
  backAbsUnary Neg (x, r) = x `glb` (-r)

  -- \| Backwards binary operations for Intervals
  backAbsBinary :: AexpBinaryOp -> (Interval, Interval, Interval) -> (Interval, Interval)
  backAbsBinary Sum (x, y, r) = (x `glb` (r - y), y `glb` (r - x))
  backAbsBinary Sub (x, y, r) = (x `glb` (r + y), y `glb` (x - r))
  backAbsBinary Mul (x, y, r) = (x `glb` (r / y), y `glb` (r / x))
  backAbsBinary Div (x, y, r) = (x `glb` (s * y), y `glb` ((x / s) `lub` 0))
   where
    s = r + mkInterval (-1) 1

  -- \| Strictly negative interval value
  negative :: Interval
  negative = mkInterval NegInf (-1)

  -- \| Non positive interval value
  nonPositive :: Interval
  nonPositive = mkInterval NegInf 0

  -- \| Strictly positive interval value
  positive :: Interval
  positive = mkInterval 1 PosInf

  -- \| Non negative interval value
  nonNegative :: Interval
  nonNegative = mkInterval 0 PosInf

-- | Type of abstract states domain for the Interval abstract value.
-- | Derived by pointwise lifing with smashed Bottom
type IntervalState = NonRelational Text Interval

-- | AbstractState instance for the IntervalState type
instance AbstractState IntervalState where
  assign :: (Text, Aexp) -> IntervalState -> IntervalState
  assign _ Bottom = Bottom
  assign (x, aexp) s = case absAexpSemantics aexp s of
    e | isBottom e -> Bottom
    e -> update s x e