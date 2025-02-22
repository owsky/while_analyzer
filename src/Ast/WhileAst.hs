module Ast.WhileAst (While (..)) where

import Ast.AexpAst (Aexp)
import Ast.BexpAst (Bexp)
import Data.Text (Text, unpack)

-- | ADT for the While programming language
data While
  = Assignment Text Aexp
  | Skip
  | Composition While While
  | IfThenElse Bexp While While
  | WhileDo Bexp While

instance Show While where
  show :: While -> String
  show w = "begin\n" <> showIndented 2 w <> "end\n"
   where
    showIndented :: Int -> While -> String
    showIndented indent (Assignment varname e) = replicate indent ' ' <> unpack varname <> " = " <> show e <> ";\n"
    showIndented indent Skip = replicate indent ' ' <> "skip;\n"
    showIndented indent (Composition c1 c2) = showIndented indent c1 <> showIndented indent c2
    showIndented indent (IfThenElse guard thenBranch elseBranch) =
      replicate indent ' '
        <> "if "
        <> show guard
        <> " then\n"
        <> showIndented (indent + 2) thenBranch
        <> replicate indent ' '
        <> "else\n"
        <> showIndented (indent + 2) elseBranch
        <> replicate indent ' '
        <> "endif;\n"
    showIndented indent (WhileDo guard body) =
      replicate indent ' '
        <> "while "
        <> show guard
        <> " do\n"
        <> showIndented (indent + 2) body
        <> replicate indent ' '
        <> "done;\n"