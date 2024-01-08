module GoalDriven.Solver.Types where

import           Data.Text      (Text)
import qualified Data.Text as T

data Step state =
    Step { name     :: !Text
         , requires :: !(state -> Bool)
         , provides :: !(state -> Maybe state)
         }

instance Show (Step state) where
    show = T.unpack . name

newtype Path s =
    Path [(s, Step s)]