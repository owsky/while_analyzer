module Abstract.State (AbstractState (..), NonRelational (..)) where

import Abstract.Domain (AbstractDomain (..))
import Ast.AexpAst (Aexp)
import Data.Map.Strict (Map)
import Data.Text (Text)
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
