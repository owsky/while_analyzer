module Utils (showMapVars, decrMaybe) where

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

-- | Decreases a Maybe Int, if it's not Nothing
decrMaybe :: Maybe Int -> Maybe Int
decrMaybe Nothing = Nothing
decrMaybe (Just x) = Just $ x - 1
