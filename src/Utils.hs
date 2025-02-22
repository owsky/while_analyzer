module Utils (showMapVars) where

-- | Show function for pairs of showables
showMapVars :: (Show k, Show v) => [(k, v)] -> String
showMapVars [] = ""
showMapVars [(k, v)] = removeQuotes $ show k <> ": " <> show v
showMapVars ((k, v) : xs) = removeQuotes $ show k <> ": " <> show v <> ", " <> showMapVars xs

-- | Removes double quotes wrapping strings, if any
removeQuotes :: String -> String
removeQuotes [] = ""
removeQuotes (x : xs)
  | x == '"' = removeQuotes xs
  | otherwise = x : removeQuotes xs
