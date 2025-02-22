module Ast.ProgramAst where

import Ast.WhileAst (While)

-- | ADT for a While Program, made of an abstract input state parameterized by an abstract domain,
-- | if given, and a While statement
data Program = Program While