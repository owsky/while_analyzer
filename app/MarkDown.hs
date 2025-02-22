module MarkDown (generateOutput) where

import Abstract.State (NonRelational)
import Ast.BexpAst (Bexp)
import Ast.WhileAst (While)
import Control.Monad.Writer (MonadWriter (tell), Writer, execWriter)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (toUpper)
import Data.Set (Set)
import Data.Set qualified as Set (toList)
import Data.Text (Text)
import Interval.ExtendedInt (ExtendedInt)
import Interval.Interval (Interval)
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
codeBlock :: String -> MarkdownWriter
codeBlock program = tell $ "```pascal\n" ++ program ++ "```\n"

-- | Generate a block for the input/output abstract state
stateBlock :: String -> String -> MarkdownWriter
stateBlock kind state = do
  tell $ "**" <> kind <> " state**: "
  tell state
  tell "\n"

-- | Generate a block for the loop invariants
loopInvariantsBlock :: [(String, String)] -> MarkdownWriter
loopInvariantsBlock l = do
  tell "**Abstract loop invariants**:"
  tell $ foldr (\(inv, guard) acc -> "\n_Loop guard_: $" <> guard <> "$\n_Loop invariant_: " <> inv <> "\n" <> acc) "" l

-- | Generate the string MarkDown containing the analysis' results, wrapped in the IO monad
generateOutput ::
  FilePath -> -- input file path, used to create the title
  NonRelational Text a -> -- input state
  (ExtendedInt, ExtendedInt) -> -- runtime bounds
  While -> -- program
  NonRelational Text a -> -- output state
  Set (NonRelational Text Interval, Bexp) -> -- loop invariants
  IO String
generateOutput inputFilePath inputState bounds program outputState loopInvariants = do
  return $ execWriter generateMarkdown
 where
  generateMarkdown = do
    header 1 $ capitalize (dropExtensions $ takeFileName inputFilePath)
    stateBlock "Input" $ show inputState
    boundsBlock bounds
    codeBlock $ show program
    stateBlock "Output" $ show outputState
    loopInvariantsBlock $ map (bimap show show) (Set.toList loopInvariants)

  capitalize [] = []
  capitalize (x : xs) = toUpper x : xs