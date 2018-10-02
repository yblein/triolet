module Eval(evaluate) where

import Game

evaluate :: Board -> Move -> Int
evaluate board move = scoreFor board move + if any isBis $ map fst move then 30 else 0
