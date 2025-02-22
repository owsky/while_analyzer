module Parser.UtilsParser where

import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix))
import Data.Char (isAlphaNum)
import Data.Set qualified as Set (fromList)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec (
  MonadParsec (takeWhileP),
  Parsec,
  State,
  Stream (Token),
  between,
  choice,
  notFollowedBy,
  setParserState,
  try,
 )
import Text.Megaparsec.Char (alphaNumChar, hspace1, lowerChar, space1, string, upperChar)
import Text.Megaparsec.Char.Lexer qualified as L

-- | Helper type which parses strings without transformers
type Parser = Parsec Void Text

-- | Space consumer, does not consume newlines
sc :: Parser ()
sc = L.space hspace1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

-- | Space consumer, consumes newlines
sc' :: Parser ()
sc' = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

-- | Consumes any trailing whitespace after parsing the given parser.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc'

-- | Consumes any trailing whitespace after parsing the given character
symbol :: Text -> Parser Text
symbol = L.symbol sc'

-- | Parses content between round braces
roundParens :: Parser a -> Parser a
roundParens = between (symbol "(") (symbol ")")

-- | Parses the comma character
comma :: Parser Text
comma = symbol ","

-- | Parses the semicolon character
semi :: Parser Text
semi = symbol ";"

-- | Creates a polymorphic prefix operator
prefix :: forall a. Text -> (a -> a) -> Operator Parser a
prefix name f = Prefix (f <$ symbol name)

-- | Creates a polymorphic, left-associative, binary operator
binaryL :: forall a. Text -> (a -> a -> a) -> Operator Parser a
binaryL name f = InfixL (f <$ symbol name)

-- | Parses a decimal number
decimal :: Parser Int
decimal = lexeme L.decimal

-- | Parses a signed integer
signedInt :: Parser Int
signedInt = L.signed sc decimal

-- | Tries to parse the given word, making sure that it's either followed by whitespace or an end of input
pWord :: Text -> Parser Text
pWord word = lexeme (string word <* notFollowedBy alphaNumChar)

-- | Tries to parse a a variable name, making sure it's not a reserved keyword
pVarName :: Parser Text
pVarName = try $ do
  vName <- pAlphaNum
  case unpack vName `elem` reservedKeywords of
    True -> fail $ "Parsed variable name " <> show vName <> " which is equal to a reserved keyword"
    False -> return vName
 where
  reservedKeywords =
    Set.fromList
      ["if", "then", "else", "endif", "while", "do", "done", "skip", "true", "false", "begin", "end", "not", "and", "or"]

-- | Tries to parse a contiguous sequence of letters and numbers, starting with a letter
pAlphaNum :: Parser Text
pAlphaNum = lexeme $ do
  firstChar <- choice [lowerChar, upperChar]
  rest <- takeWhileP Nothing isAlphaNum
  return $ pack (firstChar : unpack rest)

-- | Given a parser state, set it and consume the input once again until a newline character is reached,
-- | then it fails with a missing semicolon error message
failMissingSemi :: (Token s ~ Char, MonadParsec e s m, MonadFail m) => State s e -> m b
failMissingSemi s = do
  setParserState s
  _ <- takeWhileP Nothing (/= '\n')
  fail "Missing semicolon"