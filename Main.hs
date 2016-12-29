import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Debug.Trace

import Game
import Utils

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Up _ pos) gs = handleTileClick (posToCoord pos) gs
    where posToCoord (x, y) = (7 + (truncate (x+25)) `div` 50, 7 + (truncate (y+25)) `div` 50)
handleEvent _ gs = gs

handleTileClick :: Coord -> GameState -> GameState
handleTileClick _ gs@(GameState _ _ players _ True) = traceShow players $ gs
handleTileClick _ gs@(GameState board _ players currentPlayer False) =
  traceShow (players, scoreFor board bestMove, bestMove) $ playMove gs bestMove
  where bestMove = maximumOn (scoreFor board) $ legalMoves board $ snd $ Seq.index players currentPlayer

-- Drawing functions
drawContext :: GameState -> Picture
drawContext (GameState board _ _ _ _) = Scale 50 50 $ Translate (-7.5) (-7.5) $ Pictures [drawGrid, drawTiles board]

drawGrid = Pictures [Translate 0 15 $ Rotate 90 $ drawLines, drawLines]
  where drawLines = Pictures $ map (\x -> Line [(x, 0), (x, 15)]) [0..15]

drawTiles board = Pictures $ map drawTile $ Map.assocs board
  where drawTile ((x, y), t) = Translate (fromIntegral x) (fromIntegral y) $ Scale 0.006 0.006 $ Text $ showTile t

showTile :: Tile -> String
showTile t
  | t <= 15   = show t
  | otherwise = "*"

main = do
  setStdGen $ mkStdGen 4
  bag <- initBag
  play
    (InWindow "Triolet" (150, 150) (0, 0))
    white
    1
    (initContext bag)
    drawContext
    handleEvent
    (flip const) --(\delta world -> world)
