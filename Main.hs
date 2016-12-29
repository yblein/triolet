import Control.Arrow
import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Random.Shuffle
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Control.Monad.State
import Data.Array.IArray
import Data.Tuple
import Data.Ord

boardsize = 15
trioSum = 15
trioCount = 3
trioBonus = 15
trioletBonus = 0
nbPlayers = 4
rackSize = 3

--tilesCount = [9, 9, 8, 8, 7, 8, 6, 6, 4, 4, 3, 3, 2, 2, 1, 1, 2]
tilesCount = [9, 9, 8, 8, 7, 8, 6, 6, 4, 4, 3, 3, 2, 2, 1, 1]

type Tile = Int

type Coord = (Int, Int)
type Board = Map Coord Tile
type Bag = [Tile]
type Rack = [Tile]
type Move = [(Coord, Tile)]

data Context = Context
  { board :: Board
  , bag :: Bag
  , racks :: Array Int Rack
  , currentPlayer :: Int
  }

initBag :: Bag
initBag = concat $ zipWith replicate tilesCount [0..]

initRacks :: State Bag [Rack]
initRacks = replicateM nbPlayers $ state $ splitAt rackSize

initContext :: Bag -> Context
initContext bag = Context Map.empty bag' racksArray 0
  where
    (racks, bag') = runState initRacks bag
    racksArray = listArray (0, nbPlayers - 1) racks

powerset :: [a] -> [[a]]
powerset = filterM $ const [True, False]

permsSumLE15 :: Rack -> [[Tile]]
permsSumLE15 = nub . concatMap permutations . filter valid . init . powerset
  where valid l = sum l <= trioSum && ((length l == trioCount) `implies` (sum l == trioSum))

legalMoves :: Board -> Rack -> [Move]
legalMoves board rack = concatMap (rec board []) $ traceShowId $ permsSumLE15 rack
  where
    rec board move [] = [move]
    rec board move (t:ts) = concatMap (\c -> rec (Map.insert c t board) ((c, t):move) ts) validCoords
      where validCoords = filter (\c -> playable board c t && aligned board (c:(map fst move))) allCoords
    allCoords = liftM2 (,) [0..boardsize - 1] [0..boardsize - 1]

aligned :: Board -> [Coord] -> Bool
aligned board coords = any aligned' [coords, map swap coords]
  where
    aligned' coords = allEq (map fst coords) && (cont || around)
      where
        xs' = sort $ map snd coords
        ds = zipWith (-) (tail xs') xs'
        cont = all (== 1) ds
        around = ds == [2] && Map.member (mid (head coords) (last coords)) board
        mid (ax, ay) (bx, by) = (ax + (bx - ax) `div` 2, ay + (by - ay) `div` 2)

inBoard :: Coord -> Bool
inBoard (x, y) = x >= 0 && x < boardsize && y >= 0 && y < boardsize

allEq xs = all (== head xs) (tail xs)

playable :: Board -> Coord -> Tile -> Bool
playable board coord@(x, y) tile =
  inBoard coord && empty && (firstTile || (adj && nbAlignLEMax && sumAlignLEMax && trio && firstSquares))
  where
    empty = Map.notMember coord board
    firstTile = coord == (boardsize `div` 2, boardsize `div` 2)
    nbH = alignHor coord board (const 1) + 1
    nbV = alignVer coord board (const 1) + 1
    nbAlignLEMax = nbH <= trioCount && nbV <= trioCount
    sumH = alignHor coord board id + tile
    sumV = alignVer coord board id + tile
    sumAlignLEMax = sumH <= trioSum && sumV <= trioSum
    trio = ((nbH == trioCount) `implies` (sumH == trioSum)) && ((nbV == trioCount) `implies` (sumV == trioSum))
    adj = nbH >= 2 || nbV >= 2
    firstSquares = (Map.size board == 3) `implies` (not $ nbH == 2 && nbV == 2)
                -- TODO: this is not sufficient (see example #5)
                && (Map.size board == 8) `implies` (not $ nbH == 3 && nbV == 3)

alignHor :: Coord -> Board -> (Tile -> Int) -> Int
alignHor c b cost = sum $ map (alignDir c b cost . first) [pred, succ]

alignVer :: Coord -> Board -> (Tile -> Int) -> Int
alignVer c b cost = sum $ map (alignDir c b cost . second) [pred, succ]

alignDir :: Coord -> Board -> (Tile -> Int) -> (Coord -> Coord) -> Int
alignDir coord board cost next = sumAlignDir' (next coord)
  where
    sumAlignDir' coord =
      case Map.lookup coord board of
        Just t -> (cost t) + (sumAlignDir' (next coord))
        Nothing -> 0

implies :: Bool -> Bool -> Bool
implies a b = not a || b

scoreFor :: Board -> Move -> Int
scoreFor board [] = 0
scoreFor board move =
  if onlyFirst then
    snd (head move)
  else if allEq (map fst coords) then
    head (sums alignVer) + sum (sums (alignHor))
  else
    head (sums alignHor) + sum (sums alignVer)
  where
    board' = foldl (\b (c, t) -> Map.insert c t b) board move
    onlyFirst = Map.size board' == 1
    coords = map fst move
    sums fdir = map (sum' fdir) move
    sum' fdir (c, t) = s' + (if s' == trioSum && nb == trioCount then trioBonus else 0)
      where s = fdir c board' id
            s' = s + (if nb > 1 then t else 0)
            nb = 1 + fdir c board' (const 1)

handleEvent :: Event -> Context -> Context
handleEvent (EventKey (MouseButton LeftButton) Up _ pos) ctx =
  handleTileClick (posToCoord pos) ctx
    where
      posToCoord (x, y) = (7 + (truncate (x+25)) `div` 50, 7 + (truncate (y+25)) `div` 50)
handleEvent _ ctx = ctx

maximumOn f = fst . maximumBy (comparing snd) . map (\x -> (x, f x))

-- assume that the move is valid (i.e. respect game constraints and the player owns the played tiles)
playMove :: Context -> Move -> Context
playMove ctx@(Context board bag racks currentPlayer) move = Context board' bag' racks' currentPlayer'
  where
    board' = foldl (\b (c, t) -> Map.insert c t b) board move
    (newTiles, bag') = splitAt (length move) bag
    currentPlayer' = (currentPlayer + 1) `mod` (length racks)
    racks' = racks // [(currentPlayer, newTiles ++ (racks ! currentPlayer \\ map snd move))]


handleTileClick :: Coord -> Context -> Context
handleTileClick coord ctx@(Context board bag racks currentPlayer) =
  traceShow (scoreFor board bestMove, bestMove) $ playMove ctx bestMove
  where bestMove = maximumOn (scoreFor board) $ legalMoves board $ racks ! currentPlayer

-- Drawing functions
drawContext :: Context -> Picture
drawContext (Context board _ _ _) = Scale 50 50 $ Translate (-7.5) (-7.5) $ Pictures [drawGrid, drawTiles board]

drawGrid = Pictures [Translate 0 15 $ Rotate 90 $ drawLines, drawLines]
  where drawLines = Pictures $ map (\x -> Line [(x, 0), (x, 15)]) [0..15]

drawTiles board = Pictures $ map drawTile $ Map.assocs board
  where drawTile ((x, y), t) = Translate (fromIntegral x) (fromIntegral y) $ Scale 0.006 0.006 $ Text $ showTile t

showTile :: Tile -> String
showTile t
  | t <= 15   = show t
  | otherwise = "*"


main = do
  setStdGen $ mkStdGen 3
  bag <- shuffleM initBag
  play
    (InWindow "Triolet" (150, 150) (0, 0))
    white
    1
    (initContext bag)
    drawContext
    handleEvent
    (flip const) --(\delta world -> world)
