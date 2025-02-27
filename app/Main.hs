module Main (main) where

import Abstract.Domain (AbstractDomain)
import Abstract.State (NonRelational (Bottom, NonRelational), completeState)
import Abstract.While (absWhileSemantics)
import Args (Args (..), getArgs)
import Ast.ProgramAst (Program (..))
import Ast.WhileAst (freeVars, getConstants)
import Control.DeepSeq (deepseq)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (pack, toLower)
import Domains.Error.Error (Error (NoError))
import Domains.Error.Product (ProductError (ProductError), splitDomain)
import Domains.Interval.Bounds (setBounds)
import MarkDown (generateOutput)
import Parser.IntervalParser (pInterval)
import Parser.ProgramParser (parseProgram)
import System.FilePath (replaceExtension, takeExtension)
import Prelude hiding (lines, null, readFile)

main :: IO ()
main = do
  -- get input arguments
  args <- getArgs
  let inputFilePath = pathArg args
  let m = mArg args
  let n = nArg args
  let widenDelay = wideningDelayArg args
  let descendSteps = descendingStepsArg args
  let absDomainName = abstractDomainArg args

  -- set the interval bounds
  setBounds m n

  -- compute the output file path
  outputFilePath <- computeOutputFilePath inputFilePath

  -- parse the input file into a program
  let abstractParser = getAbstractDomainParser absDomainName
  Program inputState w <- parseProgram inputFilePath abstractParser

  -- compute the free variables occurring in the program
  let vars = freeVars w

  -- compute an abstract state containing all variables, setting missing variables to top
  let completeInputState = completeState (fromMaybe (NonRelational True Map.empty) inputState) vars

  -- transform the input state into a product state to keep track of runtime errors
  let productInputState = getProd completeInputState

  -- compute the arithmetic constants syntactically occurring in the program, used for widening thresholds
  let constants = getConstants w

  -- compute the output state and the loop invariants by induction on the program's denotational semantics
  let (output, loopInvariants) = absWhileSemantics constants widenDelay descendSteps w productInputState

  -- remove error tracking information from the loop invariants
  let loopInvariants' = Map.map (fst . splitDomain) loopInvariants

  -- separate the output abstract state into a value state and an error state
  let (outputState, outputErrors) = splitDomain output

  -- generate the analysis results in markdown format
  mdContent <- generateOutput inputFilePath completeInputState (m, n) w outputState outputErrors loopInvariants'

  -- write output to file, while forcing strict evaluation so an empty file is not created in case of errors
  mdContent `deepseq` writeFile outputFilePath mdContent
 where
  -- \| Compute the output file path by replacing the .while extension with a .md
  computeOutputFilePath path
    | takeExtension path == ".while" = return $ replaceExtension path ".md"
    | otherwise = fail $ "Input file has the wrong file extension. Expected: .while, got: " ++ takeExtension path
  -- \| Associate each domain name to its own abstract value parser
  getAbstractDomainParser domainName = case toLower $ pack domainName of
    "interval" -> pInterval
    _ -> error "Unsupported abstract domain"
  -- \| Augment the given abstract state with error tracking information through a product domain
  getProd :: (Show a, AbstractDomain a) => NonRelational k a -> NonRelational k (ProductError a)
  getProd Bottom = Bottom
  getProd (NonRelational _ x) = NonRelational False $ Map.map (`ProductError` NoError) x