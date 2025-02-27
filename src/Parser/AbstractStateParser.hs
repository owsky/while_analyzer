module Parser.AbstractStateParser (pAbstractState) where

import Abstract.Domain (AbstractDomain, bottom, top)
import Abstract.State (NonRelational (..))
import Control.Monad (void)
import Data.Map.Strict (fromList)
import Data.Text (Text)
import Parser.UtilsParser (Parser, comma, failMissingSemi, pVarName, pWord, sc', semi, symbol)
import Text.Megaparsec (MonadParsec (getParserState, label), choice, optional, sepEndBy, (<?>))

-- | Abstract state parser
pAbstractState :: (AbstractDomain a, Show a, Ord a) => Parser a -> Parser (NonRelational Text a)
pAbstractState pAbstractVal = label "abstract state" $ do
  void sc'
  s <- getParserState -- due to lazy evaluation the parser state is only acquired when failing
  void $ pWord "var"
  absVars <- pAbstractVariable pAbstractVal `sepEndBy` comma
  semicolon <- optional semi
  case semicolon of
    Just _ -> return $ NonRelational True (fromList absVars)
    Nothing -> failMissingSemi s

-- | Abstract variable parser
pAbstractVariable :: (AbstractDomain a) => Parser a -> Parser (Text, a)
pAbstractVariable pAbstractVal = label "abstract variable" $ do
  varName <- pVarName
  void $ symbol ":"
  absValue <- choice [pTop, pBot, pAbstractVal]
  return (varName, absValue)

-- | Abstract value for top
pTop :: (AbstractDomain a) => Parser a
pTop = top <$ pWord "top" <?> "abstract top"

-- | Abstract value for bottom
pBot :: (AbstractDomain a) => Parser a
pBot = bottom <$ pWord "bot" <?> "abstract bottom"