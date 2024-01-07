module GoalDriven.Solver.Types where

import           Data.Set       (Set)
import           Data.Text      (Text)
import qualified Data.Text as T

newtype Achieved g =
    Achieved (Set g)
        deriving (Semigroup, Monoid)

newtype Path g =
    Path [(Achieved g, Step g)]
        deriving (Semigroup, Monoid)

data Step g =
    Step { name        :: !Text
         , requires    :: !(Set g)
         , provides    :: !(Set g)
         , invalidates :: !(Set g)
         }

instance Show (Step g) where
    show = T.unpack . name
