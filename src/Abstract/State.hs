module Abstract.State (AbstractState (..), NonRelational (..), completeState) where

import Abstract.Domain (AbstractDomain (..))
import Abstract.Value (AbstractValue)
import Ast.AexpAst (Aexp)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Text (Text)
import Interval.ExtendedInt (ExtendedInt)
import State (State (..))
import Utils (showMapVars)
import Prelude hiding (lookup)

-- | Type class for the abstract domain of states
class (AbstractDomain a) => AbstractState a where
  -- | Abstraction of the assignment operator which given a pair
  -- | (variable name, arithmetic expression) and an abstract state,
  -- | produces a new abstract state with the evaluated expression
  -- | assigned to the given variable
  assign :: (Text, Aexp) -> a -> a

-- | Type of non relational states domains, obtained through pointwise lifting of the
-- | abstract values lattice, with a a smashed bottom
data NonRelational k v = (Show k, Show v, Eq k, Eq v, Ord k, Ord v) => NonRelational (Map k v) | Bottom

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
instance (AbstractDomain a, Show a, Eq a, Ord a) => State NonRelational Text a where
  lookup :: NonRelational Text a -> Text -> a
  lookup Bottom _ = bottom
  lookup (NonRelational s) v = fromJust $ Map.lookup v s

  update :: NonRelational Text a -> Text -> a -> NonRelational Text a
  update Bottom _ _ = Bottom
  update (NonRelational s) k v
    | isBottom v = Bottom
    | otherwise = NonRelational $ Map.insert k v s

  getVars :: NonRelational Text a -> [Text]
  getVars Bottom = []
  getVars (NonRelational s) = Map.keys s

-- | Making the type of non relational states domain an abstract domain
instance (AbstractValue a, Show a, Eq a, Ord a) => AbstractDomain (NonRelational Text a) where
  -- \| Partial order relation for abstract states, defined by checking if for all variables
  -- \| the values stored in the first state are all smaller or equal to the ones stored
  -- \| in the second state
  leq :: NonRelational Text a -> NonRelational Text a -> Bool
  leq Bottom _ = True
  leq _ Bottom = False
  leq x y = all (\v -> lookup x v `leq` lookup y v) (getVars x)

  bottom :: NonRelational Text a
  bottom = Bottom

  -- \| The top element of an abstract state should simply output the abstract values domain's
  -- \| top value for each variable, but this is not representable as a map in Haskell
  -- \| Since it's not actually needed by the analyzer, it is left undefined
  top :: NonRelational Text a
  top = undefined

  -- \| Least upper bound operator for abstract states. Bottom is absorbing
  -- \| Otherwise defined as the map intersection with the abstract value's lub operator
  lub :: NonRelational Text a -> NonRelational Text a -> NonRelational Text a
  lub Bottom y = y
  lub x Bottom = x
  lub (NonRelational x) (NonRelational y) = intersectionWith lub x y

  -- \| Greatest lower bound operator for abstract states. Bottom is strict.
  -- \| Otherwise defined as the map intersection with the abstract value's glb operator
  glb :: NonRelational Text a -> NonRelational Text a -> NonRelational Text a
  glb Bottom _ = Bottom
  glb _ Bottom = Bottom
  glb (NonRelational x) (NonRelational y) = intersectionWith glb x y

  -- \| Widening with thresholds operator for abstract states. Bottom is absorbing.
  -- \| Otherwise defined as the map intersection with the abstract value's widening operator
  widening :: Set ExtendedInt -> NonRelational Text a -> NonRelational Text a -> NonRelational Text a
  widening _ Bottom y = y
  widening _ x Bottom = x
  widening thresholds (NonRelational x) (NonRelational y) = intersectionWith (widening thresholds) x y

  -- \| Narrowing operator for abstract states. Bottom is absorbing.
  -- \| Otherwise defined as the map intersection with the abstract value's narrowing operator
  narrowing :: NonRelational Text a -> NonRelational Text a -> NonRelational Text a
  narrowing Bottom _ = Bottom
  narrowing x Bottom = x
  narrowing (NonRelational x) (NonRelational y) = intersectionWith narrowing x y

-- | Helper function which performs the intersection of two maps, using the given function to
-- | combine the values from both states. If any value ends up being bottom, then the whole
-- | state becomes bottom (smashed bottom)
intersectionWith ::
  (Show k, Show v, Ord k, Ord v, AbstractDomain v) => (a -> b -> v) -> Map k a -> Map k b -> NonRelational k v
intersectionWith f m1 m2 =
  let newState = NonRelational $ Map.intersectionWith f m1 m2
  in smashBottom newState

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

-- | If a state containing an abstract value's bottom is passed, then the output state
-- | will be bottom, otherwise it acts as the identity function
smashBottom :: (Ord k, AbstractDomain v) => NonRelational k v -> NonRelational k v
smashBottom Bottom = Bottom
smashBottom (NonRelational s) = case any isBottom (Map.elems s) of
  True -> Bottom
  False -> NonRelational s