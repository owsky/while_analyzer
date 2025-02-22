module Args (Args (..), getArgs) where

import Interval.ExtendedInt (ExtendedInt (..))
import Options.Applicative (
  Parser,
  ParserInfo,
  ReadM,
  auto,
  eitherReader,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  optional,
  progDesc,
  strArgument,
  strOption,
  value,
  (<**>),
 )
import Text.Read (readMaybe)

-- | App input arguments
data Args = Args
  { pathArg :: FilePath -- Source code file path
  , mArg :: ExtendedInt -- Left boundary for intervals
  , nArg :: ExtendedInt -- Right boundary for intervals
  , wideningDelayArg :: Maybe Int -- Widening delay, if Nothing then widening is not applied
  , descendingStepsArg :: Maybe Int -- Narrowing steps, if Nothing then narrowing is not applied
  , abstractDomainArg :: String -- Which abstract domain to use for the analysis
  }

-- | Parse either an integer, or +inf or -inf
parseExtendedInt :: ReadM ExtendedInt
parseExtendedInt = eitherReader $ \s ->
  case readMaybe s of
    Just v -> Right v
    Nothing -> Left $ "Invalid value: " ++ s ++ " (expected an integer, -inf, or +inf)"

-- | Input arguments parser
argsParser :: Parser Args
argsParser =
  Args
    <$> strArgument (metavar "PATH" <> help "Input program file path")
    <*> option parseExtendedInt (long "m" <> metavar "EXT_INT" <> help "Integer value, -inf, or +inf" <> value NegInf)
    <*> option parseExtendedInt (long "n" <> metavar "EXT_INT" <> help "Integer value, -inf, or +inf" <> value PosInf)
    <*> optional (option auto (long "widening_delay" <> metavar "INT" <> help "Positive integer for widening delay"))
    <*> optional (option auto (long "descending_steps" <> metavar "INT" <> help "Positive integer for descending steps"))
    <*> strOption (long "abstract_domain" <> metavar "STRING" <> help "Abstract domain" <> value "abstract_domain")

-- | Input arguments parser info
opts :: ParserInfo Args
opts =
  info
    (argsParser <**> helper)
    (fullDesc <> progDesc "Static analyzer for non-relational, numerical domains" <> header "While Analyzer")

-- | Run the input arguments parser
getArgs :: IO Args
getArgs = execParser opts