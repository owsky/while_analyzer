module Parser.AexpParser (pAexp) where

import Ast.AexpAst (Aexp (..), AexpBinaryOp (..), AexpUnaryOp (..))
import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Parser.UtilsParser (Parser, binaryL, pVarName, prefix, roundParens, signedInt)
import Text.Megaparsec (MonadParsec (hidden), choice, (<?>))

-- | Arithmetic expressions parser
pAexp :: Parser Aexp
pAexp = makeExprParser pTerm operatorTable

-- | Arithmetic expressions terms
pTerm :: Parser Aexp
pTerm = choice [hidden $ roundParens pAexp, pVar, pInt]

-- | Arithmetic operator table
operatorTable :: [[Operator Parser Aexp]]
operatorTable =
  [ [prefix "-" (AexpUnary Neg)]
  ,
    [ binaryL "*" (AexpBinary Mul)
    , binaryL "/" (AexpBinary Div)
    ]
  ,
    [ binaryL "+" (AexpBinary Sum)
    , binaryL "-" (AexpBinary Sub)
    ]
  ]

-- | Parser for artihmetic variables
pVar :: Parser Aexp
pVar = Var <$> pVarName <?> "variable name"

-- | Parser for arithmetic values
pInt :: Parser Aexp
pInt = Val <$> signedInt <?> "arithmetic value"
