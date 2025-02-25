module Ast.WhileAst (While (..), getConstants, freeVars) where

import Ast.AexpAst (Aexp, getConstantsAexp)
import Ast.BexpAst (Bexp, getConstantsBexp)
import Data.Set (Set, empty)
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Interval.ExtendedInt (ExtendedInt (..))

-- | ADT for the While programming language
data While
  = Assignment Text Aexp
  | Skip
  | Composition While While
  | IfThenElse Bexp While While
  | WhileDo Int Bexp While -- the integer is only used to identify the loop in the program

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
        <> "if ("
        <> show guard
        <> ") then\n"
        <> showIndented (indent + 2) thenBranch
        <> replicate indent ' '
        <> "else\n"
        <> showIndented (indent + 2) elseBranch
        <> replicate indent ' '
        <> "endif;\n"
    showIndented indent (WhileDo loopId guard body) =
      replicate indent ' '
        <> "while ("
        <> show guard
        <> ") do // ("
        <> show loopId
        <> ")\n"
        <> showIndented (indent + 2) body
        <> replicate indent ' '
        <> "done;\n"

-- | Extracts the set of numerical constants syntactically appearing in the given
-- | While program, with the inclusion of positive infinite and negative infinite
getConstants :: While -> Set ExtendedInt
getConstants w =
  let whileConstants = getConstants' empty w
      infiniteConstants = Set.fromList [PosInf, NegInf]
  in Set.union (Set.map ExtInt whileConstants) infiniteConstants
 where
  getConstants' acc (Assignment _ e) = Set.union (getConstantsAexp e) acc
  getConstants' acc Skip = acc
  getConstants' acc (Composition c1 c2) = Set.union (getConstants' acc c1) (getConstants' acc c2)
  getConstants' acc (IfThenElse b c1 c2) =
    let guardConstants = getConstantsBexp b
        thenConstants = getConstants' acc c1
        elseConstants = getConstants' acc c2
    in Set.unions [guardConstants, thenConstants, elseConstants]
  getConstants' acc (WhileDo _ b c) =
    let guardConstants = getConstantsBexp b
        bodyConstants = getConstants' acc c
    in Set.union guardConstants bodyConstants

-- | Extracts the set of variable names occurring in the given While program
freeVars :: While -> Set Text
freeVars w = freeVars' w empty
 where
  freeVars' :: While -> Set Text -> Set Text
  freeVars' (Assignment x _) acc = Set.insert x acc
  freeVars' Skip acc = acc
  freeVars' (Composition s1 s2) acc = freeVars' s1 acc `Set.union` freeVars' s2 acc
  freeVars' (IfThenElse _ s1 s2) acc = freeVars' s1 acc `Set.union` freeVars' s2 acc
  freeVars' (WhileDo _ _ s) acc = freeVars' s acc