module MarkDown (generateOutput) where

import Abstract.State (NonRelational)
import Ast.WhileAst (While (..))
import Control.Monad.Writer (MonadWriter (tell), Writer, execWriter)
import Data.Char (toUpper)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Interval.ExtendedInt (ExtendedInt)
import System.FilePath (dropExtensions, takeFileName)

type MarkdownWriter = Writer String ()

-- | Generate a markdown header at the given importance
header :: Int -> String -> MarkdownWriter
header level text = tell $ replicate level '#' ++ " " ++ text ++ "\n\n"

-- | Generate a block containing the interval boundaries
boundsBlock :: (ExtendedInt, ExtendedInt) -> MarkdownWriter
boundsBlock (m, n) = do
  tell "**Input interval boundaries**: "
  tell $ "m = " <> show m
  tell $ ", n = " <> show n
  tell "\n"

-- | Generate a code block containing the program with syntax highlighting
-- | and the loop invariants
codeBlock :: While -> MarkdownWriter
codeBlock program = do
  tell $ "```pascal\n" ++ show program ++ "```\n"

-- | Generate a block for the input/output abstract state
stateBlock :: String -> String -> MarkdownWriter
stateBlock kind state = do
  tell $ "**" <> kind <> " state**: "
  tell state
  tell "\n"

invariantsBlock :: Map Int (NonRelational Text a) -> MarkdownWriter
invariantsBlock invariants = do
  tell "**Abstract loop invariants**:\n"
  tell $ foldr (\(loopId, inv) acc -> "- (" <> show loopId <> "): " <> show inv <> "\n" <> acc) "" $ Map.assocs invariants
  tell "\n"

-- | Generate the string MarkDown containing the analysis' results, wrapped in the IO monad
generateOutput ::
  FilePath -> -- input file path, used to create the title
  NonRelational Text a -> -- input state
  (ExtendedInt, ExtendedInt) -> -- runtime bounds
  While -> -- program
  NonRelational Text a -> -- output state
  Map Int (NonRelational Text a) -> -- loop invariants
  IO String
generateOutput inputFilePath inputState bounds program outputState loopInvariants = do
  return $ execWriter generateMarkdown
 where
  generateMarkdown = do
    header 1 $ capitalize (dropExtensions $ takeFileName inputFilePath)
    stateBlock "Input" $ show inputState
    boundsBlock bounds
    codeBlock program
    invariantsBlock loopInvariants
    stateBlock "Output" $ show outputState

  capitalize [] = []
  capitalize (x : xs) = toUpper x : xs