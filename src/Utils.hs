module Utils where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Ord

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (x:xs) = all (== x) xs

implies :: Bool -> Bool -> Bool
implies a b = not a || b

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = fst . maximumBy (comparing snd) . map (id &&& f)

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort
