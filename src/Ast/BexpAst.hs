module Ast.BexpAst (Bexp (..), BoolUnaryOp (..), AexpCompOp (..), BoolBinaryOp (..), deMorgan) where

import Ast.AexpAst (Aexp)

-- | ADT for Boolean expressions
data Bexp
  = BTrue
  | BFalse
  | BexpUnary BoolUnaryOp Bexp
  | AexpComp AexpCompOp Aexp Aexp
  | BexpBinary BoolBinaryOp Bexp Bexp
  deriving (Eq, Ord)

-- | ADT for unary Boolean operators, namely logical Not
data BoolUnaryOp = Not deriving (Eq, Ord)

instance Show BoolUnaryOp where
  show :: BoolUnaryOp -> String
  show Not = "not "

-- | ADT for arithmetic comparison operators
data AexpCompOp
  = Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  deriving (Eq, Ord)

instance Show AexpCompOp where
  show :: AexpCompOp -> String
  show Eq = " == "
  show Neq = " != "
  show Lt = " < "
  show Le = " <= "
  show Gt = " > "
  show Ge = " >= "

-- | ADT for binary Boolean operators
data BoolBinaryOp = And | Or deriving (Eq, Ord)

instance Show BoolBinaryOp where
  show :: BoolBinaryOp -> String
  show And = " and "
  show Or = " or "

instance Show Bexp where
  show :: Bexp -> String
  show BTrue = "true"
  show BFalse = "false"
  show (BexpUnary Not b) = show Not <> "(" <> show b <> ")"
  show (AexpComp op a1 a2) = show a1 <> show op <> show a2
  show (BexpBinary op b1 b2) = show b1 <> show op <> show b2

-- | Given an operator for comparing arithmetic expressions,
-- | return the logically inverted operator
flipAexpOp :: AexpCompOp -> AexpCompOp
flipAexpOp Eq = Neq
flipAexpOp Neq = Eq
flipAexpOp Lt = Ge
flipAexpOp Le = Gt
flipAexpOp Gt = Le
flipAexpOp Ge = Lt

-- | Given a binary Boolean operator, return the logically
-- | inverted operator
flipBoolBinaryOp :: BoolBinaryOp -> BoolBinaryOp
flipBoolBinaryOp And = Or
flipBoolBinaryOp Or = And

-- | Applies DeMorgan's laws to eliminate negations
deMorgan :: Bexp -> Bexp
deMorgan (BexpUnary Not b) = case b of
  BTrue -> BFalse
  BFalse -> BTrue
  BexpUnary Not _ -> b
  AexpComp op a1 a2 -> AexpComp (flipAexpOp op) a1 a2
  BexpBinary op b1 b2 -> BexpBinary (flipBoolBinaryOp op) (BexpUnary Not b1) (BexpUnary Not b2)
deMorgan b = b