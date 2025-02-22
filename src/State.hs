module State (State (..)) where

-- | Type class for States, parameterized on
-- | s: the type of the actual state
-- | k: the type of the keys
-- | v: the type of the values
class State s k v where
  -- | Given a state and a key, return the value stored
  -- | for the key. Assuming totality for lookup, so
  -- | retrieving a missing key would produce a runtime error
  lookup :: s k v -> k -> v

  -- | Given a state, a key and a value, update the value stored
  -- | for the key with the new value. If the key was not stored
  -- | in the state, a new entry is created instead
  update :: s k v -> k -> v -> s k v

  -- | Return the list of variables currently stored in the state
  getVars :: s k v -> [k]