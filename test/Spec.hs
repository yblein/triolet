module Spec where

import Test.Hspec
import Data.List (permutations)
import qualified Data.HashMap.Strict as Map

import Game

main :: IO ()
main = hspec $ do
  describe "move validation" $ do
    mapM_ examplePlayable examples

    it "forbids creating a 2x2 square" $ do
      let board = Map.fromList [((6, 6), 0), ((7, 6), 0), ((6, 7), 0)]
      let move = [((7, 7), 0)]
      move `shouldNotSatisfy` validMove board

    it "accepts a triolet as first move" $ do
      let board = Map.empty
      let move = [((6, 7), 5), ((7, 7), 5), ((8, 7), 5)]
      move `shouldSatisfy` validMove board

  describe "score computation" $ do
    mapM_ exampleMatchScore examples

    it "doubles first tile on the first move" $ do
      let board = Map.empty
      let move = [((6, 7), 5), ((7, 7), 5)]
      scoreFor board move `shouldBe` 15

    it "returns 60 + triolet bonus for a triolet as first move" $ do
      let board = Map.empty
      let move = [((6, 7), 5), ((7, 7), 5), ((8, 7), 5)]
      scoreFor board move `shouldBe` 60 + trioletBonus

exampleMatchScore (i, tiles, move, score) =
  it ("matches example " ++ show i) $ scoreFor (Map.fromList tiles) move `shouldBe` score

examplePlayable (i, tiles, move, score) =
  it ("accepts example " ++ show i) $ move `shouldSatisfy` validMove (Map.fromList tiles)

-- | These are scoring examples extracted from the game rules. The tuples have the form
-- (example number, tiles on the board, tiles to play, expected score)
examples =
  [ (1, [((5,5),10)], [((6,5),3)], 13)
  , (2, [((5,5),10), ((6,5),3), ((5,4),1), ((5,6),4)], [((6,6),9)], 25)
  , (3, [((5,5),10), ((6,5),3), ((5,4),1), ((5,6),4), ((6,6),9)], [((4,6),2)], 30)
  , (4, [((5,5),10), ((6,5),3), ((5,4),1), ((5,6),4), ((6,6),9), ((4,6),2)], [((6,4),3)], 34)
  , (5, [((4,7),8), ((5,7),7), ((6,6),6), ((6,5),9)], [((6,7),0)], 60)
  , (6, [((5,6),9), ((6,6),5)], [((4,7),7), ((5,7),6)], 28)
  , (7, [((4,8),10), ((5,8),3)], [((4,7),1), ((4,9),4)], 30)
  , (8, [((4,7),15), ((4,8),0)], [((5,7),0), ((6,7),0)], 30)
  , (9, [((4,7),6), ((5,7),7), ((5,8),1), ((5,9),7)], [((6,8),12), ((6,9),0)], 32)
  , (10, [((5,5),11)], [((6,5),fixedJoker 1), ((6,6),14)], 25)
  , (11, [((5,8),7), ((6,8),6)], [((4,7),5), ((4,8),2)], 37)
  , (12, [((4,7),4), ((4,8),8)], [((4,9),fixedJoker 3), ((5,9),10)], 40)
  , (14, [((5,7),5), ((6,6),2), ((6,5),12)], [((4,7),9), ((6,7),1)], 60)
  , (15, [((4,7),9), ((5,7),4), ((6,8),7), ((5,9),3)], [((6,7),2), ((6,9),6)], 69)
  , (16, [((4,7),10), ((5,7),3)], [((4,8),4), ((5,8),8), ((6,8),3)], 55 + trioletBonus)
  , (17, [((4,7),11), ((5,7),4)], [((6,7),0), ((6,8),10), ((6,9),5)], 60 + trioletBonus)
  , (18, [((4,7),11), ((5,7),4)], [((6,7),0), ((6,8),fixedJoker 10), ((6,9),5)], 60)
  , (20, [((4,7),4), ((5,7),8), ((4,8),7), ((5,8),6), ((5,9),1)], [((6,7),3), ((6,8),2), ((6,9),10)], 101 + trioletBonus)
  , (24, [((5,7),2), ((6,7),8), ((7,8),7)], [((7,7),5)], 72)
  , (26, [], [((7,7),9), ((8,7),5)], 23)
  , (28, [((5,7),12), ((6,7),2)], [((7,7),1)], 60)
  , (30, [((7,8),10), ((8,7),13)], [((7,7),fixedJoker 1), ((7,6), 4)], 73)
  , (33, [((5,7),1), ((6,7),8), ((7,8),8)], [((7,7),6), ((7,9),1)], 90)
  , (35, [((7,5),3), ((8,5),2), ((7,6),5), ((8,6),8), ((6,7),3)], [((7,7),7), ((8,7),5)], 120)
  ]
