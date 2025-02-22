module Ast.WhileAst (While (..), getConstants, freeVars, countLoops) where

import Ast.AexpAst (Aexp, getConstantsAexp)
import Ast.BexpAst (Bexp, getConstantsBexp)
import Data.Set (Set, empty)
import Data.Set qualified as Set
import Data.Text (Text)
import Interval.ExtendedInt (ExtendedInt (..))

-- | ADT for the While programming language
data While
  = Assignment Text Aexp
  | Skip
  | Composition While While
  | IfThenElse Bexp While While
  | WhileDo Int Bexp While -- the integer is only used to identify the loop in the program

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

-- | Assigns incremental integer IDs to each loop occurring in the program
countLoops :: While -> While
countLoops w = fst $ countLoops' (w, 0)
 where
  countLoops' (WhileDo _ b s, c) =
    let (s', c') = countLoops' (s, c + 1)
    in (WhileDo c b s', c')
  countLoops' (IfThenElse b s1 s2, c) =
    let (s1', c') = countLoops' (s1, c)
        (s2', c'') = countLoops' (s2, c')
    in (IfThenElse b s1' s2', c'')
  countLoops' (Composition s1 s2, c) =
    let (s1', c') = countLoops' (s1, c)
        (s2', c'') = countLoops' (s2, c')
    in (Composition s1' s2', c'')
  countLoops' p = p