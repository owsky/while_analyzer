module Parser.WhileParser (pWhile) where

import Ast.WhileAst (While (..))
import Control.Monad (void)
import Data.Bifunctor qualified
import Parser.AexpParser (pAexp)
import Parser.BexpParser (pBexp)
import Parser.UtilsParser (Parser, failMissingSemi, pVarName, pWord, sc', semi, symbol)
import Text.Megaparsec (MonadParsec (getParserState, label), choice, optional)

-- | Parser for statements of the While programming language
pWhile :: Parser While
pWhile = fst <$> pWhile' 0

pWhile' :: Int -> Parser (While, Int)
pWhile' c = label "while statement" $ do
  void sc'
  s <- getParserState -- due to lazy evaluation the parser state is only acquired when failing
  (statement, c') <- choice [pIfThenElse c, pSkip c, pWhileDo c, pAssignment c]
  semicolon <- optional semi
  case semicolon of
    Just _ -> do
      c2m <- optional $ pWhile' c'
      return $ maybe (statement, c') (Data.Bifunctor.first (Composition statement)) c2m
    Nothing -> failMissingSemi s

-- | Parser for assignment statements
pAssignment :: Int -> Parser (While, Int)
pAssignment c = label "assignment statement" $ do
  varName <- pVarName
  _ <- symbol "="
  value <- pAexp
  return (Assignment c varName value, c + 1)

-- | Parser for skip statements
pSkip :: Int -> Parser (While, Int)
pSkip c = label "skip statement" $ do
  _ <- pWord "skip"
  return (Skip c, c + 1)

-- | Parser for if then else statements
pIfThenElse :: Int -> Parser (While, Int)
pIfThenElse c = label "if then else statement" $ do
  _ <- pWord "if"
  guard <- pBexp
  _ <- pWord "then"
  (thenBranch, c') <- pWhile' $ c + 1
  _ <- pWord "else"
  (elseBranch, c'') <- pWhile' c'
  _ <- pWord "endif"
  return (IfThenElse c guard thenBranch elseBranch, c'')

-- | Parser for While loops
pWhileDo :: Int -> Parser (While, Int)
pWhileDo c = label "while do statement" $ do
  _ <- pWord "while"
  guard <- pBexp
  _ <- pWord "do"
  (body, c') <- pWhile' $ c + 1
  _ <- pWord "done"
  return (WhileDo c guard body, c')
