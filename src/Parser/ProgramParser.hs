module Parser.ProgramParser (parseProgram) where

import Abstract.Value (AbstractValue)
import Ast.ProgramAst (Program (..))
import Ast.WhileAst (countLoops)
import Control.Monad (void)
import Data.Text (pack)
import Parser.AbstractStateParser (pAbstractState)
import Parser.UtilsParser (Parser, pWord)
import Parser.WhileParser (pWhile)
import Text.Megaparsec (eof, errorBundlePretty, optional, parse, (<?>))

-- | Attempts to parse the text file pointed by the given file path into a program
parseProgram ::
  (Ord a, Show a, AbstractValue a) => FilePath -> Parser a -> IO (Program a)
parseProgram inputFilePath pAbstractVal = do
  fileContents <- readFile inputFilePath
  let parsed = parse (pProgram pAbstractVal <* eof) "" $ pack fileContents
  case parsed of
    Left bundle -> fail $ errorBundlePretty bundle
    Right prog -> return prog

-- | Parser for While programs parameterized on the Intervals abstract domain
pProgram :: (Ord a, Show a, AbstractValue a) => Parser a -> Parser (Program a)
pProgram pAbstractVal = do
  absState <- optional (pAbstractState pAbstractVal) <?> "abstract state definition"
  void $ pWord "begin"
  body <- pWhile <?> "program body"
  void $ pWord "end"
  -- before returning the While program, assign proper IDs to each while loop
  return $ Program absState $ countLoops body