{-# LANGUAGE TupleSections #-}

module Game
  ( Tile, Move, Board, Bag, Rack, Coord, Player(..), GameState(..)
  , freeJoker, fixedJoker, isJoker
  , boardSize, allCoords, trioletBonus
  , initGame, playMove, playChangeAll, playPass, scoreFor, legalMoves, validMove
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

maxScorelessTurns = 3

tilesCount :: [Int]
tilesCount = [9, 9, 8, 8, 7, 8, 6, 6, 4, 4, 3, 3, 2, 2, 1, 1, 2]

midBoard = (m, m) where m = boardSize `div` 2

type Tile = Int
type Coord = (Int, Int)
type Board = HashMap Coord Tile
type Bag = [Tile]
type Rack = [Tile]
type Move = [(Coord, Tile)]

data Player = Player
  { score :: Int
  , rack :: Rack
  , name :: String
  }

data GameState = GameState
  { board :: Board
  , bag :: Bag
  , players :: Seq Player
  , currentPlayer :: Maybe Int
  , nbNullPly :: Int
  , rng :: StdGen
  }

-- We represent a joker whose value is still free (i.e. on a rack or in the bag) by the integer 16.
-- Jokers with a fixed values (i.e. in a move or on the board) are represented by `17 + value`.

freeJoker :: Tile
freeJoker = 16

fixedJoker :: Int -> Tile
fixedJoker n = 17 + n

isJoker :: Tile -> Bool
isJoker = (>= 16)

-- The given tile must not be a free joker
tileValue :: Tile -> Int
tileValue t
  | t == freeJoker = error "free jokers have no value"
  | otherwise = if isJoker t then t - 17 else t


initBag :: Rand StdGen Bag
initBag = shuffleM $ concat $ zipWith replicate tilesCount [0..]

initRacks :: Int -> State Bag [Rack]
initRacks nbPlayers = replicateM nbPlayers $ state $ splitAt rackSize

initGame :: [String] -> StdGen -> GameState
initGame names rng =
  GameState
    { board = Map.empty
    , bag = bag'
    , players = Seq.fromList [Player { score = 0, rack, name } | (rack, name) <- zip racks names]
    , currentPlayer = Just 0
    , nbNullPly = 0
    , rng = rng'
    }
  where (bag, rng') = runRand initBag rng
        (racks, bag') = runState (initRacks $ length names) bag

legalMoves :: Board -> Rack -> [Move]
legalMoves b rack = filter (not . createsSquare b) $ filter oneJockerMax allMoves
  where
    allMoves = map sort $ rmdups $ concatMap fromCoords $ anchors b

    fromCoords c = do
      (t', ts) <- select rack
      t <- if isJoker t' then map fixedJoker [0..15] else [t']
      guard $ validTile b c t
      next <- liftM2 ($) [first, second] [succ, pred]
      zs <- [] : fromCoordsWithDir (Map.insert c t b) (next c) ts next
      return $ (c, t):zs

    fromCoordsWithDir b c tiles next
      | isFree b c = do
          (t', ts) <- select tiles
          t <- if isJoker t' then map fixedJoker [0..15] else [t']
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

oneJockerMax move = length (filter isJoker $ map snd move) <= 1

validMove :: Board -> Move -> Bool
validMove b move = allValid b move && aligned b (map fst move) && not (createsSquare b move) && oneJockerMax move
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
    (nbH, sumH) = (+1) *** (+tileValue tile) $ countNSumHor coord board
    (nbV, sumV) = (+1) *** (+tileValue tile) $ countNSumVer coord board
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
        Just t -> tileValue t:alignDir' (next coord)
        Nothing -> []

scoreFor :: Board -> Move -> Int
scoreFor _     [] = 0
scoreFor board move = baseScore + specials + if isTriolet then trioletBonus else 0
  where
    baseScore
      | onlyFirst              = snd (head move)
      | allEq (map fst coords) = minimum (sums countNSumVer) + sum (sums countNSumHor)
      | otherwise              = minimum (sums countNSumHor) + sum (sums countNSumVer)

    board' = updateBoard board move
    onlyFirst = Map.size board' == 1
    coords = map fst move
    isTriolet = length move == rackSize && not (any isJoker $ map snd move)

    sums fdir = map (sum' fdir) move
    sum' fdir (c, t)
      | isTrio = trioSum * trioMult
      | isJoker t = s
      | otherwise = s'
      where
        (n, s) = first (+1) $ fdir c board'
        s' = s + if n > 1 then tileValue t else 0
        isTrio = s' == trioSum && n == trioCount

    specials = sumSpecial isDouble 2 + sumSpecial isTripple 3
    sumSpecial p mult =
      case filter (p . fst) move of
        [] -> 0
        (coord, tile):_ -> (mult - 1) * if trio then trioSum * trioMult else (if isJoker tile then 0 else tile)
          where
            trio = (sumH == trioSum && nbH == trioCount) || (sumV == trioSum && nbV == trioCount)
            (nbH, sumH) = (+1) *** (+tileValue tile) $ countNSumHor coord board'
            (nbV, sumV) = (+1) *** (+tileValue tile) $ countNSumVer coord board'

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

-- assume that the move is valid (i.e. it respects the game constraints and the player owns the played tiles)
playMove :: GameState -> Move -> (GameState, Int)
playMove gs@(GameState { currentPlayer = Nothing }) _ = (gs, 0)
playMove gs@(GameState { currentPlayer = (Just currentPlayer) }) move =
  (gs
    { board = board'
    , bag = bag'
    , players = players'
    , currentPlayer = currentPlayer'
    , nbNullPly = nbNullPly'
    }
  , points)
  where
    board' = updateBoard (board gs) move
    player = Seq.index (players gs) currentPlayer
    hasBis = any isBis $ map fst move
    nextPlayer = if hasBis then currentPlayer else (currentPlayer + 1) `mod` nbPlayers
    currentPlayer' = if isOver then Nothing else Just nextPlayer
    (newTiles, bag') = splitAt (length move) (bag gs)
    rack' = newTiles ++ (rack player \\ map (\(_, t) -> if isJoker t then freeJoker else t) move)
    score' = score player + points
    points = scoreFor (board gs) move + if isFinished then sum $ concatMap rack players' else 0
    players' = Seq.update currentPlayer (player { score = score', rack = rack' }) (players gs)
    nbPlayers = length (players gs)
    isOver = isFinished || isStuck
    isFinished = null rack'
    isStuck = nbNullPly' >= maxScorelessTurns * nbPlayers
    nbNullPly' = if points == 0 then nbNullPly gs + 1 else 0

playChangeAll :: GameState -> GameState
playChangeAll gs@(GameState { currentPlayer = Nothing }) = gs
playChangeAll gs@(GameState { bag, players, currentPlayer = (Just currentPlayer), rng }) =
  playPass $ gs { bag = bag'', players = players', rng = rng' }
   where
    player = Seq.index players currentPlayer
    (rack', bag') = splitAt rackSize bag
    (bag'', rng') = runRand (shuffleM $ rack player ++ bag') rng
    players' = Seq.update currentPlayer (player { rack = rack' }) players

playPass :: GameState -> GameState
playPass gs@(GameState { currentPlayer = Nothing }) = gs
playPass gs@(GameState { currentPlayer = (Just currentPlayer) }) =
  gs
    { currentPlayer = if isStuck then Nothing else Just $ (currentPlayer + 1) `mod` nbPlayers
    , nbNullPly = nbNullPly'
    }
  where
    nbPlayers = length $ players gs
    isStuck = nbNullPly' >= maxScorelessTurns * nbPlayers
    nbNullPly' = nbNullPly gs + 1
