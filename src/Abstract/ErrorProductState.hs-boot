module Abstract.ErrorProductState where

import Abstract.State (NonRelational)
import Data.Text (Text)
import Domains.Error (ErrorState)

data ErrorProductState a
  = ErrorProductState
  { valueState :: NonRelational Text a
  , errorState :: ErrorState
  }