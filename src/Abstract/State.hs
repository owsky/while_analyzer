module Abstract.State (AbstractState (..), NonRelational (..), completeState) where

import Abstract.Domain (AbstractDomain (..))
import Abstract.Value (AbstractValue)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import ExtendedInt (ExtendedInt)
import ProgramState (ProgramState (..))
import Utils (showMapVars)
import Prelude hiding (lookup)

-- | Type class for the abstract domain of states
class (AbstractDomain s) => AbstractState s k v where
  -- | Abstraction of the assignment operator which given a pair
  -- | (variable name, arithmetic expression) and an abstract state,
  -- | produces a new abstract state with the evaluated expression
  -- | assigned to the given variable
  assign :: k -> v -> s -> s

-- | Type of non relational states domains, obtained through pointwise lifting of the
-- | abstract values lattice, with an optional smashed bottom depending on the Boolean value
data NonRelational k v where
  NonRelational :: (Show k, Show v, Eq k, Eq v, Ord k, Ord v) => Map k v -> NonRelational k v
  Bottom :: NonRelational k v

-- | Ord instance of non relational states domains, it holds no actual meaning
-- | as it's just required in order to create a Set of states
instance Ord (NonRelational k v) where
  compare :: NonRelational k v -> NonRelational k v -> Ordering
  compare Bottom Bottom = EQ
  compare Bottom _ = LT
  compare _ Bottom = GT
  compare (NonRelational x) (NonRelational y) = compare x y

-- | Eq instance of non relational states domains, simply checks if the maps
-- | hold the same values for the same keys
instance Eq (NonRelational k v) where
  (==) :: NonRelational k v -> NonRelational k v -> Bool
  Bottom == Bottom = True
  (NonRelational x) == (NonRelational y) = x == y
  _ == _ = False

-- | Show instance of non relational states domains, used to produce the
-- | analysis output
instance Show (NonRelational k v) where
  show :: NonRelational k v -> String
  show Bottom = "$⊥^{\\text{\\#}}$"
  show (NonRelational s) = showMapVars (Map.toList s)

-- | Making the type of non relational states domain a State
instance (AbstractDomain a, Show a, Eq a, Ord a) => ProgramState NonRelational k a where
  lookup :: NonRelational k a -> k -> a
  lookup Bottom _ = bottom
  lookup (NonRelational s) v = fromJust $ Map.lookup v s

  update :: NonRelational k a -> k -> a -> NonRelational k a
  update Bottom _ _ = Bottom
  update (NonRelational s) k v = smashBottom (NonRelational $ Map.insert k v s)

  getVars :: NonRelational k a -> [k]
  getVars Bottom = []
  getVars (NonRelational s) = Map.keys s

-- | Making the type of non relational states domain an abstract domain
instance (AbstractValue a, Show a, Eq a, Ord a) => AbstractDomain (NonRelational k a) where
  -- \| Partial order relation for abstract states, defined by checking if for all variables
  -- \| the values stored in the first state are all smaller or equal to the ones stored
  -- \| in the second state
  leq :: NonRelational k a -> NonRelational k a -> Bool
  leq Bottom _ = True
  leq _ Bottom = False
  leq x y = all (\v -> lookup x v `leq` lookup y v) (getVars x)

  bottom :: NonRelational k a
  bottom = Bottom

  -- \| The top element of an abstract state should simply output the abstract values domain's
  -- \| top value for each variable, but this is not representable as a map in Haskell
  -- \| Since it's not actually needed by the analyzer, it is left undefined
  top :: NonRelational k a
  top = undefined

  -- \| Least upper bound operator for abstract states. Bottom is absorbing
  -- \| Otherwise defined as the map intersection with the abstract value's lub operator
  lub :: NonRelational k a -> NonRelational k a -> NonRelational k a
  lub Bottom y = y
  lub x Bottom = x
  lub (NonRelational x) (NonRelational y) = intersectionWith lub x y

  -- \| Greatest lower bound operator for abstract states. Bottom is strict.
  -- \| Otherwise defined as the map intersection with the abstract value's glb operator
  glb :: NonRelational k a -> NonRelational k a -> NonRelational k a
  glb Bottom _ = Bottom
  glb _ Bottom = Bottom
  glb (NonRelational x) (NonRelational y) = intersectionWith glb x y

  -- \| Widening with thresholds operator for abstract states. Bottom is absorbing.
  -- \| Otherwise defined as the map intersection with the abstract value's widening operator
  widening :: Set ExtendedInt -> NonRelational k a -> NonRelational k a -> NonRelational k a
  widening _ Bottom y = y
  widening _ x Bottom = x
  widening thresholds (NonRelational x) (NonRelational y) = intersectionWith (widening thresholds) x y

  -- \| Narrowing operator for abstract states. Bottom is absorbing.
  -- \| Otherwise defined as the map intersection with the abstract value's narrowing operator
  narrowing :: NonRelational k a -> NonRelational k a -> NonRelational k a
  narrowing Bottom _ = Bottom
  narrowing x Bottom = x
  narrowing (NonRelational x) (NonRelational y) = intersectionWith narrowing x y

-- | Helper function which performs the intersection of two maps, using the given function to
-- | combine the values from both states. If the Boolean value is true and any value ends up being bottom,
-- | then the whole state becomes bottom (smashed bottom)
intersectionWith ::
  (Show k, Show v, Ord k, Ord v, AbstractDomain v) => (a -> b -> v) -> Map k a -> Map k b -> NonRelational k v
intersectionWith f m1 m2 = smashBottom $ NonRelational $ Map.intersectionWith f m1 m2

-- | Given an abstract state and a list of variables syntactically occuring in a program,
-- | it completes the state by adding for each missing variable an entry which is mapped
-- | to the top value
completeState :: (Ord k, AbstractDomain v) => NonRelational k v -> [k] -> NonRelational k v
completeState s vars = smashBottom $ completeState' s vars
 where
  completeState' :: (Ord k, AbstractDomain v) => NonRelational k v -> [k] -> NonRelational k v
  completeState' Bottom _ = Bottom
  completeState' s' [] = s'
  completeState' (NonRelational s') (x : xs) =
    if Map.notMember x s'
      then completeState' (NonRelational (Map.insert x top s')) xs
      else case isBottom (fromJust $ Map.lookup x s') of
        True -> Bottom
        False -> completeState' (NonRelational s') xs

-- | If a state containing an abstract value's bottom is passed and the Boolean value is true,
-- | then the output state will be bottom, otherwise it acts as the identity function
smashBottom :: (Ord k, AbstractDomain v) => NonRelational k v -> NonRelational k v
smashBottom Bottom = Bottom
smashBottom (NonRelational s) = case any isBottom (Map.elems s) of
  True -> Bottom
  False -> NonRelational s