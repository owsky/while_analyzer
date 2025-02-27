module Parser.IntervalParser where

import Control.Monad (void)
import Domains.Interval.Interval (Interval, mkInterval)
import ExtendedInt (ExtendedInt (..))
import Parser.UtilsParser (Parser, comma, pWord, signedInt, symbol)
import Text.Megaparsec (choice, label, (<?>))

-- | Abstract value for interval
pInterval :: Parser Interval
pInterval = label "interval" $ do
  void $ symbol "["
  low <- choice [pNegInf, ExtInt <$> signedInt]
  void comma
  high <- choice [pPosInf, ExtInt <$> signedInt]
  void $ symbol "]"
  return $ mkInterval low high

-- | Abstract value for negative infinite
pNegInf :: Parser ExtendedInt
pNegInf = NegInf <$ pWord "-inf" <?> "negative infinite"

-- | Abstract value for positive infinite
pPosInf :: Parser ExtendedInt
pPosInf = PosInf <$ pWord "+inf" <?> "positive infinite"
