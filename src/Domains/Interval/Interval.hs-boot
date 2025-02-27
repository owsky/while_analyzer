module Domains.Interval.Interval where

-- \| hs-boot module for Interval, necessary in order to split the Interval
-- \| module into multiple submodules without cyclic import errors

import Abstract.Domain (AbstractDomain)
import ExtendedInt (ExtendedInt)

data Interval
  = Empty
  | Interval ExtendedInt ExtendedInt

instance Eq Interval

instance AbstractDomain Interval

instance Ord Interval

mkInterval :: ExtendedInt -> ExtendedInt -> Interval