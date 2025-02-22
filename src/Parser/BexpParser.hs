module Parser.BexpParser (pBexp) where

import Ast.BexpAst (AexpCompOp (..), Bexp (..), BoolBinaryOp (..), BoolUnaryOp (..))
import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Parser.AexpParser (pAexp)
import Parser.UtilsParser (Parser, binaryL, pWord, prefix, roundParens, symbol)
import Text.Megaparsec (MonadParsec (label), choice, (<?>))

-- | Boolean expression parser
pBexp :: Parser Bexp
pBexp = makeExprParser pTerm operatorTable

-- | Boolean expressions terms
pTerm :: Parser Bexp
pTerm = choice [roundParens pBexp, pTrue, pFalse, pAexpComp]

-- | Boolean operator table
operatorTable :: [[Operator Parser Bexp]]
operatorTable =
  [ [prefix "not" (BexpUnary Not)]
  , [binaryL "and" (BexpBinary And), binaryL "or" (BexpBinary Or)]
  ]

-- | Parser for Boolean true
pTrue :: Parser Bexp
pTrue = BTrue <$ pWord "true" <?> "Boolean true"

-- | Parser for Boolean false
pFalse :: Parser Bexp
pFalse = BFalse <$ pWord "false" <?> "Boolean false"

-- | Parser for comparisons for arithmetic expressions
pAexpComp :: Parser Bexp
pAexpComp = label "Boolean binary operators" $ do
  e1 <- pAexp
  op <- choice [symbol "==", symbol "!=", symbol "<=", symbol "<", symbol ">=", symbol ">"]
  e2 <- pAexp
  case op of
    "==" -> return $ AexpComp Eq e1 e2
    "!=" -> return $ AexpComp Neq e1 e2
    "<" -> return $ AexpComp Lt e1 e2
    "<=" -> return $ AexpComp Le e1 e2
    ">" -> return $ AexpComp Gt e1 e2
    ">=" -> return $ AexpComp Ge e1 e2
    _ -> error $ "Unsupported operator: " <> show op