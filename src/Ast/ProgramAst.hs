module Ast.ProgramAst where

import Abstract.State (NonRelational)
import Ast.WhileAst (While)
import Data.Text (Text)

-- | ADT for a While Program, made of an abstract input state parameterized by an abstract domain,
-- | if given, and a While statement
data Program a = Program (Maybe (NonRelational Text a)) While