module GoalDriven.Solver (solve) where

import GoalDriven.Solver.Types

import           Data.Heap      (Entry (..))
import qualified Data.Heap as H
import           Data.Maybe     (mapMaybe)

solve :: forall s m. MonadFail m => s -> (s -> Bool) -> Int -> [Step s] -> m [Step s]
solve initial solved maxCost available = go (H.singleton (Entry 0 (Path mempty)))

    where
    go paths =

        case H.viewMin paths of

            Nothing ->
                fail "No solution found"

            Just (Entry cost path, heap') ->

                let state = getCurrentState path

                in if solved state

                       then let Path p = path in pure . reverse $ map snd p

                       else go
                          . foldr H.insert heap'
                          . filter (\(Entry c _) -> c <= maxCost)
                          . map (Entry (cost+1) . addToPath path)
                          . mapMaybe (fanOut state)
                          $ available

    addToPath :: Path s -> (s, Step s) -> Path s
    addToPath (Path path) next = Path (next:path)

    getCurrentState :: Path s -> s
    getCurrentState (Path        []) = initial
    getCurrentState (Path ((s,_):_)) = s

    fanOut :: s -> Step s -> Maybe (s, Step s)
    fanOut state step
        | requires step state =
            case provides step state of
                Nothing     -> Nothing
                Just state' -> Just (state', step)
        | otherwise = Nothing