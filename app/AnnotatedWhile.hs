module AnnotatedWhile where

import Abstract.State (NonRelational)
import Ast.AexpAst (Aexp)
import Ast.BexpAst (Bexp)
import Ast.WhileAst (While (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)

-- | ADT mirroring While statements, which replaces the integer
-- | ID identifying each while loop, with its abstract loop invariant
data AnnotatedWhile a
  = AssignmentA Text Aexp
  | SkipA
  | CompositionA (AnnotatedWhile a) (AnnotatedWhile a)
  | IfThenElseA Bexp (AnnotatedWhile a) (AnnotatedWhile a)
  | WhileDoA (NonRelational Text a) Bexp (AnnotatedWhile a)

-- | Show instsance of AnnotatedWhile, used to print the output of the analysis
instance Show (AnnotatedWhile a) where
  show :: AnnotatedWhile a -> String
  show w = "begin\n" <> showIndented 2 w <> "end\n"
   where
    showIndented :: Int -> AnnotatedWhile a -> String
    showIndented indent (AssignmentA varname e) = replicate indent ' ' <> unpack varname <> " = " <> show e <> ";\n"
    showIndented indent SkipA = replicate indent ' ' <> "skip;\n"
    showIndented indent (CompositionA c1 c2) = showIndented indent c1 <> showIndented indent c2
    showIndented indent (IfThenElseA guard thenBranch elseBranch) =
      replicate indent ' '
        <> "if ("
        <> show guard
        <> ") then\n"
        <> showIndented (indent + 2) thenBranch
        <> replicate indent ' '
        <> "else\n"
        <> showIndented (indent + 2) elseBranch
        <> replicate indent ' '
        <> "endif;\n"
    showIndented indent (WhileDoA inv guard body) =
      replicate indent ' '
        <> "while ("
        <> show guard
        <> ") do "
        <> "-- ("
        <> show inv
        <> ")\n"
        <> showIndented (indent + 2) body
        <> replicate indent ' '
        <> "done;\n"

-- | Conversion function from While programs to AnnotatedWhile programs, replaces each loop ID
-- | with the abstract state stored in the map at the corresponding loop ID
annotateWhile :: While -> Map Int (NonRelational Text a) -> AnnotatedWhile a
annotateWhile (WhileDo c b s) invs = WhileDoA (fromJust $ Map.lookup c invs) b $ annotateWhile s invs
annotateWhile (IfThenElse b s1 s2) invs = IfThenElseA b (annotateWhile s1 invs) (annotateWhile s2 invs)
annotateWhile (Composition s1 s2) invs = CompositionA (annotateWhile s1 invs) (annotateWhile s2 invs)
annotateWhile (Assignment var val) _ = AssignmentA var val
annotateWhile Skip _ = SkipA