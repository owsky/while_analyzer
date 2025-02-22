module Parser.WhileParser (pWhile) where

import Ast.WhileAst (While (..))
import Control.Monad (void)
import Parser.AexpParser (pAexp)
import Parser.BexpParser (pBexp)
import Parser.UtilsParser (Parser, failMissingSemi, pVarName, pWord, sc', semi, symbol)
import Text.Megaparsec (MonadParsec (getParserState, label), choice, optional, (<?>))

-- | Parser for statements of the While programming language
pWhile :: Parser While
pWhile = label "while statement" $ do
  void sc'
  s <- getParserState -- due to lazy evaluation the parser state is only acquired when failing
  c1 <- choice [pIfThenElse, pSkip, pWhileDo, pAssignment]
  semicolon <- optional semi
  case semicolon of
    Just _ -> do
      c2m <- optional pWhile
      return $ maybe c1 (Composition c1) c2m
    Nothing -> failMissingSemi s

-- | Parser for assignment statements
pAssignment :: Parser While
pAssignment = Assignment <$> pVarName <* symbol "=" <*> pAexp <?> "assignment statement"

-- | Parser for skip statements
pSkip :: Parser While
pSkip = Skip <$ pWord "skip" <?> "skip statement"

-- | Parser for if then else statements
pIfThenElse :: Parser While
pIfThenElse =
  IfThenElse
    <$ pWord "if"
    <*> pBexp
    <* pWord "then"
    <*> pWhile
    <* pWord "else"
    <*> pWhile
    <* pWord "endif"
      <?> "if then else statement"

-- | Parser for While loops
pWhileDo :: Parser While
pWhileDo =
  WhileDo (-1) -- assign a placeholder ID to the loop
    <$ pWord "while"
    <*> pBexp
    <* pWord "do"
    <*> pWhile
    <* pWord "done"
      <?> "while do statement"
