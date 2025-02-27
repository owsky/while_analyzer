module Domains.Interval.Arithmetics (sumIntervals, subIntervals, mulIntervals, divIntervals, negateInterval) where

import Abstract.Domain (lub)
import Domains.Interval.Bounds (clampInterval)
import {-# SOURCE #-} Domains.Interval.Interval (Interval (..), mkInterval)
import ExtendedInt (ExtendedInt (..))

-- | Addition of two intervals with strict Bottom
sumIntervals :: Interval -> Interval -> Interval
sumIntervals Empty _ = Empty
sumIntervals _ Empty = Empty
sumIntervals (Interval a b) (Interval c d) = mkInterval (a + c) (b + d)

-- | Subtraction of two intervals with strict Bottom
subIntervals :: Interval -> Interval -> Interval
subIntervals Empty _ = Empty
subIntervals _ Empty = Empty
subIntervals (Interval a b) (Interval c d) = mkInterval (a - d) (b - c)

-- | Product of two intervals with strict Bottom
mulIntervals :: Interval -> Interval -> Interval
mulIntervals Empty _ = Empty
mulIntervals _ Empty = Empty
mulIntervals (Interval a b) (Interval c d) =
  let products = [a * c, a * d, b * c, b * d]
  in mkInterval (minimum products) (maximum products)

-- | Division of two intervals with strict Bottom
divIntervals :: Interval -> Interval -> Interval
divIntervals Empty _ = Empty
divIntervals _ Empty = Empty
divIntervals (Interval a b) (Interval c d)
  -- case where the second interval is strictly positive
  | 1 <= c =
      let low = min (a / c) (a / d)
          high = max (b / c) (b / d)
      in Interval low high
  -- case where the second interval is strictly negative
  | d <= -1 =
      let low = min (b / c) (b / d)
          high = max (a / c) (a / d)
      in Interval low high
  -- case where the second interval contains zero
  | otherwise =
      -- the denominator is intersected with [1, +inf] and [-inf, -1] in order to
      -- split it into its positive and negative parts
      -- then the numerator is divided by them individually and the result
      -- will be the union of the two, approximating the division
      let low = Interval a b `divIntervals` (Interval c d `glb'` Interval 1 PosInf)
          high = Interval a b `divIntervals` (Interval c d `glb'` Interval NegInf (-1))
      in clampInterval $ low `lub` high

-- | Negation of an interval
negateInterval :: Interval -> Interval
negateInterval Empty = Empty
negateInterval (Interval a b) = mkInterval (-b) (-a)

-- | Greatest lower bound operator which does not clamp the output interval
-- | Needed to ensure convergence of the division operator
glb' :: Interval -> Interval -> Interval
glb' Empty _ = Empty
glb' _ Empty = Empty
glb' (Interval a b) (Interval c d)
  | max a c <= min b d = Interval (max a c) (min b d)
  | otherwise = Empty
