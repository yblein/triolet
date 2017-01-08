{-# LANGUAGE TupleSections #-}

module Game where

import System.Random.Shuffle
import Control.Monad.Random
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Control.Monad.State
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Tuple
import Data.List
import Control.Arrow

import Utils

boardSize = 15
trioSum = 15
trioCount = 3
trioBonus = 15
trioletBonus = 0
rackSize = 3

--tilesCount = [9, 9, 8, 8, 7, 8, 6, 6, 4, 4, 3, 3, 2, 2, 1, 1, 2]
tilesCount = [9, 9, 8, 8, 7, 8, 6, 6, 4, 4, 3, 3, 2, 2, 1, 1]

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

permsSumLE15 :: Rack -> [[Tile]]
permsSumLE15 = nub . concatMap permutations . filter valid . init . powerset
  where valid l = sum l <= trioSum && ((length l == trioCount) `implies` (sum l == trioSum))

legalMoves :: Board -> Rack -> [Move]
legalMoves board rack = concatMap (rec board []) $ permsSumLE15 rack
  where
    rec board move [] = [move]
    rec board move (t:ts) = concatMap (\c -> rec (Map.insert c t board) ((c, t):move) ts) validCoords
      where validCoords = filter (\c -> playable board c t && aligned board (c:(map fst move))) coords
            coords = case move of
              [] -> allCoords
              ((x, y), _):_ -> map (x,) [y-2..y+2] ++ map (,y) [x-2..x+2]

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

playable :: Board -> Coord -> Tile -> Bool
playable board coord@(x, y) tile =
  inBoard coord && empty && (firstTile || (adj && nbAlignLEMax && sumAlignLEMax && trio && firstSquares))
  where
    empty = not $ Map.member coord board
    firstTile = coord == (boardSize `div` 2, boardSize `div` 2)
    hs = alignHor coord board
    vs = alignVer coord board
    nbH = length hs + 1
    nbV = length vs + 1
    nbAlignLEMax = nbH <= trioCount && nbV <= trioCount
    sumH = sum hs + tile
    sumV = sum vs + tile
    sumAlignLEMax = sumH <= trioSum && sumV <= trioSum
    trio = ((nbH == trioCount) `implies` (sumH == trioSum)) && ((nbV == trioCount) `implies` (sumV == trioSum))
    adj = nbH >= 2 || nbV >= 2
    -- TODO; the rule for 2-wide squares is not sufficient if the move has more tiles
    firstSquares = (Map.size board == 3) `implies` (not $ nbH == 2 && nbV == 2)
                -- TODO: this is not sufficient (see example #5)
                && (Map.size board == 8) `implies` (not $ nbH == 3 && nbV == 3)

alignHor :: Coord -> Board -> [Int]
alignHor c b = (alignDir c b $ first succ) ++ (alignDir c b $ first pred)

alignVer :: Coord -> Board -> [Int]
alignVer c b = (alignDir c b $ second pred) ++ (alignDir c b $ second succ)

alignDir :: Coord -> Board -> (Coord -> Coord) -> [Int]
alignDir coord board next = alignDir' (next coord)
  where
    alignDir' coord =
      case Map.lookup coord board of
        Just t -> t:(alignDir' (next coord))
        Nothing -> []

scoreFor :: Board -> Move -> Int
scoreFor board [] = 0
scoreFor board move = baseScore + specials + if length move == 3 then trioletBonus else 0
  where
    baseScore =
      if onlyFirst then
        snd (head move)
      else if allEq (map fst coords) then
        head (sums alignVer) + sum (sums alignHor)
      else
        head (sums alignHor) + sum (sums alignVer)

    board' = foldl (\b (c, t) -> Map.insert c t b) board move
    onlyFirst = Map.size board' == 1
    coords = map fst move

    sums fdir = map (sum' fdir) move
    sum' fdir (c, t) = s' + if s' == trioSum && nb == trioCount then trioBonus else 0
      where ts = fdir c board'
            s = sum ts
            s' = s + if nb > 1 then t else 0
            nb = 1 + length ts

    specials = sumSpecial isDouble 2 + sumSpecial isTripple 3
    sumSpecial p mult =
      case filter (p . fst) move of
        [] -> 0
        (c@(x, y), t):_ -> (mult - 1) * if trio then trioSum * 2 else t
          where
            trio = (sumH == trioSum && nbH == trioCount) || (sumV == trioSum && nbV == trioCount)
            hs = alignHor c board'
            vs = alignVer c board'
            nbH = length hs + 1
            nbV = length vs + 1
            sumH = sum hs + t
            sumV = sum vs + t


isDouble :: Coord -> Bool
isDouble (x, y) = (y == m && dx == 4) || (x == m && dy == 4) || (dx == 3 && dy == 3) || (x, y) == (m, m)
  where (m, dx, dy) = midAndOffsets (x, y)

isTripple :: Coord -> Bool
isTripple (x, y) = (dx == 3 && dy == 6) || (dx == 6 && dy == 3)
  where (_, dx, dy) = midAndOffsets (x, y)

isBis :: Coord -> Bool
isBis (x, y) = (y == m && dx == 7) || (x == m && dy == 7) || (dx == 6 && dy == 6)
  where (m, dx, dy) = midAndOffsets (x, y)

midAndOffsets :: Coord -> (Int, Int, Int)
midAndOffsets (x, y) = (m, dx, dy)
  where
    m = boardSize `div` 2
    dx = abs $ m - x
    dy = abs $ m - y

-- assume that the move is valid (i.e. it respects the game constraints and the player owns the played tiles)
playMove :: GameState -> Move -> GameState
playMove gs@(GameState _ _ _ Nothing _) _ = gs
playMove (GameState board bag players (Just currentPlayer) rng) move = gameState'
  where
    board' = foldl (\b (c, t) -> Map.insert c t b) board move
    (score, rack) = Seq.index players currentPlayer
    hasBis = any isBis $ map fst move
    nextPlayer = if hasBis then currentPlayer else (currentPlayer + 1) `mod` (length players)
    currentPlayer' = if isOver then Nothing else Just nextPlayer
    (newTiles, bag') = splitAt (length move) bag
    rack' = newTiles ++ (rack \\ map snd move)
    score' = score + scoreFor board' move + if isOver then sum $ concatMap snd players' else 0
    players' = Seq.update currentPlayer (score', rack') players
    isOver = null rack'
    gameState' = GameState board' bag' players' currentPlayer' rng

playChangeAll :: GameState -> GameState
playChangeAll gs@(GameState _ _ _ Nothing _) = gs
playChangeAll gs@(GameState board bag players (Just currentPlayer) rng) = gameState'
  where
    gameState' = GameState board bag'' players' currentPlayer' rng'
    (score, rack) = Seq.index players currentPlayer
    currentPlayer' = Just $ (currentPlayer + 1) `mod` (length players)
    (rack', bag') = splitAt 3 bag
    players' = Seq.update currentPlayer (score, rack') players
    (bag'', rng') = runRand (shuffleM $ rack ++ bag') rng
