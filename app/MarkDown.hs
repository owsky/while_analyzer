module MarkDown (generateOutput) where

import Abstract.Semantics.While (Invariants)
import Abstract.State (NonRelational (..))
import Alarms (Alarms (..), showAlarms)
import Ast.WhileAst (While (..))
import Control.Monad.Writer (MonadWriter (tell), Writer, execWriter)
import Data.Char (toUpper)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import ExtendedInt (ExtendedInt)
import System.FilePath (dropExtensions, takeFileName)

type MarkdownWriter = Writer String ()

-- | Generate a markdown header at the given importance
header :: Int -> String -> MarkdownWriter
header level text = tell $ replicate level '#' ++ " " ++ text ++ "\n\n"

-- | Generate a block containing the interval boundaries
boundsBlock :: (ExtendedInt, ExtendedInt) -> MarkdownWriter
boundsBlock (m, n) = do
  tell "**Input interval boundaries**:\n"
  tell $ "- m = " <> show m <> ", n = " <> show n <> "\n"

-- | Generate a code block containing the program with syntax highlighting
-- | and the loop invariants
codeBlock :: While -> MarkdownWriter
codeBlock program = do
  tell $ "```pascal\n" ++ show program ++ "```\n"

-- | Generate a block for the input/output abstract state
stateBlock :: String -> String -> MarkdownWriter
stateBlock kind state = do
  tell $ "**" <> kind <> " state**:\n"
  tell $ "- " <> state <> "\n\n"

-- | Generate a block to show the invariants
invariantsBlock :: Map Int (NonRelational Text a) -> MarkdownWriter
invariantsBlock invariants = do
  let invS = foldr (\(loopId, inv) acc -> "- (" <> show loopId <> "): " <> show inv <> "\n" <> acc) "" $ Map.assocs invariants
  if null invS then tell "" else tell $ "**Abstract loop invariants**:\n" ++ invS ++ "\n"

-- | Generate a block to show the possible runtime errors detected by the analysis
errorsBlock :: Alarms -> MarkdownWriter
errorsBlock alarms = do
  tell "\n**Runtime error alarms**:\n"
  let alarmsStrings = showAlarms alarms
  case null alarmsStrings of
    True -> tell "None"
    False -> do
      tell $ showErrorList alarmsStrings
 where
  showErrorList [] = ""
  showErrorList (x : xs) = "- " <> x <> "\n" <> showErrorList xs

-- | Generate the string MarkDown containing the analysis' results, wrapped in the IO monad
generateOutput ::
  (Show a, Ord a) =>
  FilePath -> -- input file path, used to create the title
  NonRelational Text a -> -- input state
  (ExtendedInt, ExtendedInt) -> -- runtime bounds
  While -> -- program
  NonRelational Text a -> -- output state
  Alarms -> -- output errors
  Invariants a -> -- loop invariants
  IO String
generateOutput inputFilePath inputState bounds program outputState outputErrors loopInvariants = do
  -- errors = do
  return $ execWriter generateMarkdown
 where
  generateMarkdown = do
    header 1 $ capitalize (dropExtensions $ takeFileName inputFilePath)
    stateBlock "Input" $ show inputState
    boundsBlock bounds
    codeBlock program
    invariantsBlock loopInvariants
    stateBlock "Output" $ show outputState
    errorsBlock outputErrors

  capitalize [] = []
  capitalize (x : xs) = toUpper x : xs