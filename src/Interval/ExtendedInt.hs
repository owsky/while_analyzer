module Interval.ExtendedInt where

-- ADT for integers extended with positive infinite and negative infinite
data ExtendedInt = ExtInt Int | PosInf | NegInf deriving (Eq)

instance Show ExtendedInt where
  show :: ExtendedInt -> String
  show PosInf = "+∞"
  show NegInf = "-∞"
  show (ExtInt i) = show i
