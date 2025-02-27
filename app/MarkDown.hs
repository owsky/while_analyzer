module MarkDown (generateOutput) where

import Abstract.State (NonRelational (..))
import Abstract.While (Invariants)
import Ast.WhileAst (While (..))
import Control.Monad.Writer (MonadWriter (tell), Writer, execWriter)
import Data.Char (toUpper)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Domains.Error.Error (Error (..), showErrors)
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
  tell "**Abstract loop invariants**:\n"
  tell $ foldr (\(loopId, inv) acc -> "- (" <> show loopId <> "): " <> show inv <> "\n" <> acc) "" $ Map.assocs invariants
  tell "\n"

-- | Generate a block to show the possible runtime errors detected by the analysis
errorsBlock :: NonRelational Text Error -> MarkdownWriter
errorsBlock Bottom = tell "An unexpected error occurred: the error domain should never produce a $\bot^\\#$ state"
errorsBlock (NonRelational _ errors) = do
  tell "\n**Runtime error alarms**:\n"
  let errorString = showErrorList $ Map.assocs errors
  tell $ if null errorString then "None" else errorString
 where
  -- \| Pretty printing the (key,value) list
  showErrorList :: [(Text, Error)] -> String
  showErrorList [] = ""
  showErrorList ((varName, Error s) : xs) = case Set.size s == 0 of
    True -> showErrorList xs
    False -> "- " <> unpack varName <> ": " <> showErrors s <> "\n" <> showErrorList xs

-- | Generate the string MarkDown containing the analysis' results, wrapped in the IO monad
generateOutput ::
  (Show a, Ord a) =>
  FilePath -> -- input file path, used to create the title
  NonRelational Text a -> -- input state
  (ExtendedInt, ExtendedInt) -> -- runtime bounds
  While -> -- program
  NonRelational Text a -> -- output state
  NonRelational Text Error -> -- output errors
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