{-# LANGUAGE TupleSections #-}

module Game
  ( Tile, Move, Board, Bag, Rack, Coord, Player, GameState(..)
  , boardSize, allCoords, trioletBonus
  , initGame, playMove, playChangeAll, scoreFor, legalMoves, validMove
  , isDouble, isTripple, isBis
  ) where

import System.Random.Shuffle
import Control.Monad.Random
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Control.Monad.State
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.List
import Data.Foldable
import Control.Arrow

import Utils

boardSize, trioSum, trioCount, trioMult, trioletBonus, rackSize :: Int
boardSize = 15
trioSum = 15
trioCount = 3
trioMult = 2
trioletBonus = 0
rackSize = 3

tilesCount :: [Int]
tilesCount = [9, 9, 8, 8, 7, 8, 6, 6, 4, 4, 3, 3, 2, 2, 1, 1] -- and 2 jokers

midBoard = (m, m) where m = boardSize `div` 2

type Tile = Int
type Coord = (Int, Int)
type Board = HashMap Coord Tile
type Bag = [Tile]
type Rack = [Tile]
type Move = [(Coord, Tile)]
type Player = (Int, Rack)

data GameState = GameState
  { board :: Board
  , bag :: Bag
  , players :: Seq Player
  , currentPlayer :: Maybe Int
  , rng :: StdGen
  }

initBag :: Rand StdGen Bag
initBag = shuffleM $ concat $ zipWith replicate tilesCount [0..]

initRacks :: Int -> State Bag [Rack]
initRacks nbPlayers = replicateM nbPlayers $ state $ splitAt rackSize

initGame :: Int -> StdGen -> GameState
initGame nbPlayers rng = GameState Map.empty bag' (Seq.fromList $ zip (repeat 0) racks) (Just 0) rng'
  where (bag, rng') = runRand initBag rng
        (racks, bag') = runState (initRacks nbPlayers) bag

legalMoves :: Board -> Rack -> [Move]
legalMoves b rack = filter (not . createsSquare b) $ rmdups $ map sort $ concatMap fromCoords $ anchors b
  where
    fromCoords c = do
      (t, ts) <- select rack
      guard $ validTile b c t
      next <- liftM2 ($) [first, second] [succ, pred]
      zs <- [] : fromCoordsWithDir (Map.insert c t b) (next c) ts next
      return $ (c, t):zs

    fromCoordsWithDir b c tiles next
      | isFree b c = do
          (t, ts) <- select tiles
          guard $ validTile b c t
          zs <- [] : fromCoordsWithDir (Map.insert c t b) (next c) ts next
          return $ (c, t):zs
      | otherwise = do
          (_, ts) <- select tiles
          fromCoordsWithDir b (next c) ts next

    select []     = []
    select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

-- | Anchors are the cells which are both free and have at least one adjacent tile.
-- | The first turn on the game is an exception where the only anchor is the center of the board.
anchors :: Board -> [Coord]
anchors b
  | Map.null b = [midBoard]
  | otherwise = filter (isFree b) $ rmdups $ concatMap neighbours $ Map.keys b

isFree board coords = not $ Map.member coords board

neighbours (x,y) = filter inBoard [(x,y+1), (x,y-1), (x+1,y), (x-1,y)]

validMove :: Board -> Move -> Bool
validMove board move = allValid board move && aligned board (map fst move) && not (createsSquare board move)
  where
    -- the move may not be given in proper order, just check that at least one permutation is valid
    allValid board move = any (allValid' board) (permutations move)
    allValid' _ [] = True
    allValid' board ((c, t):xs) = validTile board c t && allValid' (Map.insert c t board) xs

createsSquare :: Board -> Move -> Bool
createsSquare board move = createsSqSize 2 || createsSqSize 3
  where
    createsSqSize n = nbTiles == n*n && nbXs == n && nbYs == n
    nbTiles = Map.size board + length move
    nbXs = length $ nub $ map fst coords
    nbYs = length $ nub $ map snd coords
    coords = Map.keys board ++ map fst move

allCoords :: [Coord]
allCoords = liftM2 (,) [0..boardSize - 1] [0..boardSize - 1]

aligned :: Board -> [Coord] -> Bool
aligned board coords = any aligned' [(fst, snd), (snd, fst)]
  where
    aligned' (fst', snd') = allEq (map fst' coords) && (cont || around)
      where
        xs' = sort $ map snd' coords
        ds = zipWith (-) (tail xs') xs'
        cont = all (== 1) ds
        around = ds == [2] && Map.member (mid (head coords) (last coords)) board
        mid (ax, ay) (bx, by) = (ax + (bx - ax) `div` 2, ay + (by - ay) `div` 2)

inBoard :: Coord -> Bool
inBoard (x, y) = x >= 0 && x < boardSize && y >= 0 && y < boardSize

validTile :: Board -> Coord -> Tile -> Bool
validTile board coord tile =
  inBoard coord && isFree board coord && (firstTile || (adj && gameConstraints))
  where
    firstTile = coord == midBoard
    (nbH, sumH) = (+1) *** (+tile) $ countNSumHor coord board
    (nbV, sumV) = (+1) *** (+tile) $ countNSumVer coord board
    gameConstraints = constrDir (nbH, sumH) && constrDir (nbV, sumV)
    constrDir (nb, sum) = nb <= trioCount && sum <= trioSum && ((nb == trioCount) `implies` (sum == trioSum))
    adj = nbH >= 2 || nbV >= 2

countNSumHor :: Coord -> Board -> (Int, Int)
countNSumHor c b = length &&& sum $ alignDir c b (first succ) ++ alignDir c b (first pred)

countNSumVer :: Coord -> Board -> (Int, Int)
countNSumVer c b = length &&& sum $ alignDir c b (second pred) ++ alignDir c b (second succ)

alignDir :: Coord -> Board -> (Coord -> Coord) -> [Int]
alignDir coord board next = alignDir' (next coord)
  where
    alignDir' coord =
      case Map.lookup coord board of
        Just t -> t:alignDir' (next coord)
        Nothing -> []

scoreFor :: Board -> Move -> Int
scoreFor _     [] = 0
scoreFor board move = baseScore + specials + if length move == 3 then trioletBonus else 0
  where
    baseScore
      | onlyFirst              = snd (head move)
      | allEq (map fst coords) = head (sums countNSumVer) + sum (sums countNSumHor)
      | otherwise              = head (sums countNSumHor) + sum (sums countNSumVer)

    board' = updateBoard board move
    onlyFirst = Map.size board' == 1
    coords = map fst move

    sums fdir = map (sum' fdir) move
    sum' fdir (c, t) = s' * if s' == trioSum && n == trioCount then trioMult else 1
      where (n, s) = first (+1) $ fdir c board'
            s' = s + if n > 1 then t else 0

    specials = sumSpecial isDouble 2 + sumSpecial isTripple 3
    sumSpecial p mult =
      case filter (p . fst) move of
        [] -> 0
        (coord, tile):_ -> (mult - 1) * if trio then trioSum * trioMult else tile
          where
            trio = (sumH == trioSum && nbH == trioCount) || (sumV == trioSum && nbV == trioCount)
            (nbH, sumH) = (+1) *** (+tile) $ countNSumHor coord board'
            (nbV, sumV) = (+1) *** (+tile) $ countNSumVer coord board'


isDouble :: Coord -> Bool
isDouble (x, y) = (dy == 0 && dx == 4) || (dx == 0 && dy == 4) || (dx == 3 && dy == 3) || (dx, dy) == (0, 0)
  where (dx, dy) = dists (x, y)

isTripple :: Coord -> Bool
isTripple (x, y) = (dx == 3 && dy == 6) || (dx == 6 && dy == 3)
  where (dx, dy) = dists (x, y)

isBis :: Coord -> Bool
isBis (x, y) = (dy == 0 && dx == 7) || (dx == 0 && dy == 7) || (dx == 6 && dy == 6)
  where (dx, dy) = dists (x, y)

dists :: Coord -> (Int, Int)
dists (x, y) = (abs $ m - x, abs $ m - y)
  where m = boardSize `div` 2

updateBoard :: Board -> Move -> Board
updateBoard = foldl' (\b (c, t) -> Map.insert c t b)

isStuck :: GameState -> Bool
isStuck (GameState board bag players _ _) = not anyTileValid
  where
    anyTileValid = or [validTile board coord tile | coord <- anchors board, tile <- allTiles]
    allTiles = concatMap snd (toList players) ++ bag

-- assume that the move is valid (i.e. it respects the game constraints and the player owns the played tiles)
playMove :: GameState -> Move -> (GameState, Int)
playMove gs@(GameState _ _ _ Nothing _) _ = (gs, 0)
playMove (GameState board bag players (Just currentPlayer) rng) move = (gameState', points)
  where
    board' = updateBoard board move
    (score, rack) = Seq.index players currentPlayer
    hasBis = any isBis $ map fst move
    nextPlayer = if hasBis then currentPlayer else (currentPlayer + 1) `mod` length players
    currentPlayer' = if isOver then Nothing else Just nextPlayer
    (newTiles, bag') = splitAt (length move) bag
    rack' = newTiles ++ (rack \\ map snd move)
    score' = score + points
    points = scoreFor board move + if isFinished then sum $ concatMap snd players' else 0
    players' = Seq.update currentPlayer (score', rack') players
    isOver = isFinished || isStuck gameState'
    isFinished = null rack'
    gameState' = GameState board' bag' players' currentPlayer' rng

playChangeAll :: GameState -> GameState
playChangeAll gs@(GameState _ _ _ Nothing _) = gs
playChangeAll (GameState board bag players (Just currentPlayer) rng) = gameState'
  where
    gameState' = GameState board bag'' players' currentPlayer' rng'
    (score, rack) = Seq.index players currentPlayer
    currentPlayer' = Just $ (currentPlayer + 1) `mod` length players
    (rack', bag') = splitAt 3 bag
    players' = Seq.update currentPlayer (score, rack') players
    (bag'', rng') = runRand (shuffleM $ rack ++ bag') rng
