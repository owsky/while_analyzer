module Domains.Interval.Bounds (clampInterval, setBounds) where

import Abstract.Domain (AbstractDomain (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import {-# SOURCE #-} Domains.Interval.Interval (Interval (..))
import ExtendedInt (ExtendedInt (..))
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (lookup)

-- | Unsafe pattern used to store and retrieve the global bounds for intervals.
-- | Since Haskell does not support dependent types, I adopted this solution.
-- | Maybe there are better (safe, pure) alternatives, I mostly did not
-- | want to change the abstract domain's type classes' function signatures
-- | to accomodate a specific instance's requirements
{-# NOINLINE globalBounds #-}
globalBounds :: IORef (ExtendedInt, ExtendedInt)
globalBounds = unsafePerformIO $ newIORef (NegInf, PosInf)

-- | Sets the global bounds for intervals
setBounds :: ExtendedInt -> ExtendedInt -> IO ()
setBounds m n = writeIORef globalBounds (m, n)

-- | Checks if a bound is outside the [m, n] interval
outsideBoundsInt :: ExtendedInt -> Bool
outsideBoundsInt NegInf = True
outsideBoundsInt PosInf = True
outsideBoundsInt x =
  let (m, n) = unsafePerformIO $ readIORef globalBounds
  in x < m || n < x

-- | Clamps the given interval so that, if any of its bounds are outside
-- | the domain's global bounds, the best correct approximation of it
-- | is returned, otherwise acts as the identity function
clampInterval :: Interval -> Interval
clampInterval (Interval (ExtInt a) (ExtInt b))
  -- constant intervals are always allowed
  | a == b = Interval (ExtInt a) (ExtInt b)
  -- if both bounds are not representable then top is returned
  | outsideBoundsInt (ExtInt a) && outsideBoundsInt (ExtInt b) = top
  -- if the left or the right bound is not representable then
  -- it is pushed to +inf/-inf respectively
  | outsideBoundsInt (ExtInt a) = Interval NegInf (ExtInt b)
  | outsideBoundsInt (ExtInt b) = Interval (ExtInt a) PosInf
clampInterval (Interval NegInf (ExtInt b))
  | outsideBoundsInt (ExtInt b) = top
  | otherwise = Interval NegInf (ExtInt b)
clampInterval (Interval (ExtInt a) PosInf)
  | outsideBoundsInt (ExtInt a) = top
  | otherwise = Interval (ExtInt a) PosInf
clampInterval i = i