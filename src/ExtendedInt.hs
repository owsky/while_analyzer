module ExtendedInt where

import Text.Read (readMaybe)

-- ADT for integers extended with positive infinite and negative infinite
data ExtendedInt = ExtInt Int | PosInf | NegInf deriving (Eq)

instance Show ExtendedInt where
  show :: ExtendedInt -> String
  show PosInf = "+∞"
  show NegInf = "-∞"
  show (ExtInt i) = show i

-- | Num instance of the extended int type, needed to use arithmetic
-- | operators
instance Num ExtendedInt where
  -- \| Standard additions, with possibility of runtime errors
  -- \| if two opposing sign infinites are added. This should
  -- \| never occur with the interval domain arithmetics
  (+) :: ExtendedInt -> ExtendedInt -> ExtendedInt
  ExtInt x + ExtInt y = ExtInt (x + y)
  ExtInt _ + PosInf = PosInf
  ExtInt _ + NegInf = NegInf
  PosInf + ExtInt _ = PosInf
  NegInf + ExtInt _ = NegInf
  PosInf + PosInf = PosInf
  NegInf + NegInf = NegInf
  _ + _ = error "Undefined infinite sum"

  -- \| Same as the addition operator
  (-) :: ExtendedInt -> ExtendedInt -> ExtendedInt
  ExtInt x - ExtInt y = ExtInt (x - y)
  ExtInt _ - PosInf = NegInf
  ExtInt _ - NegInf = PosInf
  PosInf - ExtInt _ = PosInf
  NegInf - ExtInt _ = NegInf
  PosInf - NegInf = PosInf
  NegInf - PosInf = NegInf
  _ - _ = error "Undefined infinite subtraction"

  -- \| Standard integer product, with the exception of infinite * 0
  -- \| being equal to 0, needed to handle cases such as
  -- \| [1, +inf] * [0, 1] = [0, +inf]
  (*) :: ExtendedInt -> ExtendedInt -> ExtendedInt
  ExtInt x * ExtInt y = ExtInt (x * y)
  ExtInt 0 * _ = ExtInt 0
  _ * ExtInt 0 = ExtInt 0
  PosInf * PosInf = PosInf
  PosInf * NegInf = NegInf
  NegInf * NegInf = PosInf
  NegInf * PosInf = NegInf
  ExtInt x * PosInf
    | x > 0 = PosInf
    | otherwise = NegInf
  ExtInt x * NegInf
    | x > 0 = NegInf
    | otherwise = PosInf
  PosInf * ExtInt y
    | y > 0 = PosInf
    | otherwise = NegInf
  NegInf * ExtInt y
    | y > 0 = NegInf
    | otherwise = PosInf

  negate :: ExtendedInt -> ExtendedInt
  negate (ExtInt x) = ExtInt (-x)
  negate PosInf = NegInf
  negate NegInf = PosInf

  -- These functions are not actually needed by the analyzer but
  -- they are required by the type class

  abs :: ExtendedInt -> ExtendedInt
  abs (ExtInt x) = ExtInt $ abs x
  abs _ = error "Abstract value is not defined for infinites"

  signum :: ExtendedInt -> ExtendedInt
  signum (ExtInt x) = ExtInt $ signum x
  signum PosInf = ExtInt 1
  signum NegInf = ExtInt (-1)

  fromInteger :: Integer -> ExtendedInt
  fromInteger = ExtInt . fromIntegral

-- | Ord instance of the ExtendedInt type, needed to use minimum
-- | and maximum functions
instance Ord ExtendedInt where
  (<=) :: ExtendedInt -> ExtendedInt -> Bool
  ExtInt x <= ExtInt y = x <= y
  ExtInt _ <= PosInf = True
  ExtInt _ <= NegInf = False
  PosInf <= ExtInt _ = False
  NegInf <= ExtInt _ = True
  PosInf <= PosInf = True
  PosInf <= NegInf = False
  NegInf <= PosInf = True
  NegInf <= NegInf = True

-- | Fractional instance of the ExtendedInt type, needed
-- | to use the division operator
instance Fractional ExtendedInt where
  -- \| Non-standard division as anything divided by infinite
  -- \| results in zero. Needed to keep consistency with the
  -- \| multiplication operator as:
  -- \| inf / inf = inf * (1 / inf) = inf * 0 = 0
  (/) :: ExtendedInt -> ExtendedInt -> ExtendedInt
  ExtInt _ / PosInf = 0
  ExtInt _ / NegInf = 0
  PosInf / PosInf = 0
  _ / ExtInt y | y == 0 = error "Division by zero"
  ExtInt x / ExtInt y = ExtInt (x `div` y)
  PosInf / ExtInt y
    | y > 0 = PosInf
    | otherwise = NegInf
  NegInf / ExtInt y
    | y > 0 = NegInf
    | otherwise = PosInf
  PosInf / NegInf = 0
  NegInf / PosInf = 0
  NegInf / NegInf = 0

  -- These functions are not actually needed by the analyzer but
  -- they are required by the type class

  recip :: ExtendedInt -> ExtendedInt
  recip = (1 /)

  fromRational :: Rational -> ExtendedInt
  fromRational = error "While Analyzer only supports the integers data type"

-- | Read instance fo the ExtendedInt type, needed in order to
-- | parse the numerical bounds from the CLI arguments
instance Read ExtendedInt where
  readsPrec :: Int -> ReadS ExtendedInt
  readsPrec _ "-inf" = [(NegInf, "")]
  readsPrec _ "+inf" = [(PosInf, "")]
  readsPrec _ str = case readMaybe str of
    Just n -> [(ExtInt n, "")]
    Nothing -> []