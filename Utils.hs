module Utils where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Ord

powerset :: [a] -> [[a]]
powerset = filterM $ const [True, False]

allEq :: Eq a => [a] -> Bool
allEq xs = all (== head xs) (tail xs)

implies :: Bool -> Bool -> Bool
implies a b = not a || b

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = fst . maximumBy (comparing snd) . map (id &&& f)

