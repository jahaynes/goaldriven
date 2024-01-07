module GoalDriven.Solver where

import GoalDriven.Solver.Types

import           Data.Heap      (Entry (..), Heap)
import qualified Data.Heap as H
import           Data.Set       (Set, (\\))
import qualified Data.Set as S

solve :: forall g m. (MonadFail m, Ord g) => Int -> Set g -> [Step g] -> m [Step g]
solve maxDepth goals available = go 0 (H.singleton (Entry 0 mempty))

    where
    go :: MonadFail m => Int -> Heap (Entry Int (Path g)) -> m [Step g]
    go depth paths
        | depth > maxDepth = fail "Too many steps"
        | otherwise =
            case H.viewMin paths of
                Nothing -> fail "Impossible"
                Just (Entry cost path, heap')
                    | solved path -> let Path p = path in pure . reverse $ map snd p
                    | otherwise -> go (depth + 1)
                                 . foldr (H.insert . Entry (cost + 1)) heap'
                                 . fanOut
                                 $ path

    solved :: Path g -> Bool
    solved (Path                 []) = null goals
    solved (Path ((Achieved a,_):_)) = goals `S.isSubsetOf` a

    fanOut :: Path g -> [Path g]
    fanOut (Path []) = map (\s -> Path [(provideAndInvalidate mempty s, s)])
                     . filter (null . requires)
                     $ available

    fanOut (Path (p@(a@(Achieved ach), _):ps)) = map (\s -> Path ((provideAndInvalidate a s, s):p:ps))
                                               . filter (\s -> requires s `S.isSubsetOf` ach)
                                               $ available

    provideAndInvalidate :: Ord g => Achieved g -> Step g -> Achieved g
    provideAndInvalidate (Achieved a) s = Achieved ((a <> provides s) \\ invalidates s)
