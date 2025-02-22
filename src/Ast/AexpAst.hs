module Ast.AexpAst (Aexp (..), AexpUnaryOp (..), AexpBinaryOp (..), getConstantsAexp) where

import Data.Set (Set, empty)
import Data.Set qualified as Set
import Data.Text (Text, unpack)

-- | ADT for arithmetic expressions
data Aexp
  = Var Text
  | Val Int
  | AexpUnary AexpUnaryOp Aexp
  | AexpBinary AexpBinaryOp Aexp Aexp
  deriving (Eq, Ord)

-- | ADT for arithmetic unary operators, namely negation
data AexpUnaryOp = Neg deriving (Show, Eq, Ord)

-- | ADT for arithmetic binary operators
data AexpBinaryOp
  = Sum
  | Sub
  | Mul
  | Div
  deriving (Show, Eq, Ord)

instance Show Aexp where
  show :: Aexp -> String
  show (Var varname) = unpack varname
  show (Val n) = show n
  show (AexpUnary Neg x) = case x of
    Val n -> show (-n)
    Var v -> "-" <> unpack v
    _ -> "-(" <> show x <> ")"
  show (AexpBinary op a1 a2) =
    let opStr = case op of
          Sum -> " + "
          Sub -> " - "
          Mul -> " * "
          Div -> " / "
    in show a1 <> opStr <> show a2

-- | Compute the set of numerical constants syntactically
-- | appearing in the arithmetic expression
getConstantsAexp :: Aexp -> Set Int
getConstantsAexp = getConstantsAexp' empty
 where
  getConstantsAexp' acc (Var _) = acc
  getConstantsAexp' acc (Val n) = Set.insert n acc
  getConstantsAexp' acc (AexpUnary Neg a) = case a of
    (Val n) -> Set.insert (-n) acc
    _ -> getConstantsAexp' acc a
  getConstantsAexp' acc (AexpBinary _ a1 a2) = Set.union (getConstantsAexp' acc a1) (getConstantsAexp' acc a2)