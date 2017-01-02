--import Graphics.Gloss
--import Graphics.Gloss.Interface.Pure.Game

import Graphics.UI.Gtk hiding (rectangle) -- (fill, background)
import Graphics.Rendering.Cairo
import Control.Monad.Trans (liftIO)

import Control.Monad
import Data.IORef
import Data.Foldable
import System.Random
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Debug.Trace

import Game
import Utils

playAI gs@(GameState _ _ players Nothing) = gs
playAI gs@(GameState board _ players (Just currentPlayer)) = playMove gs bestMove
  where bestMove = maximumOn (scoreFor board) $ legalMoves board $ snd $ Seq.index players currentPlayer

{-
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Up _ pos) gs = handleTileClick (posToCoord pos) gs
    where posToCoord (x, y) = (7 + (truncate (x+25)) `div` 50, 7 + (truncate (y+25)) `div` 50)
handleEvent _ gs = gs
-}

main = do
  setStdGen $ mkStdGen 4
  game <- initBag >>= (newIORef . initContext)

  initGUI
  window <- windowNew
  set window [windowTitle := "Triolet" ]
  on window deleteEvent $ liftIO mainQuit >> return False
  drawingArea <- drawingAreaNew
  containerAdd window drawingArea
  widgetShowAll window

  on drawingArea buttonPressEvent $ do
    liftIO $ modifyIORef game playAI
    liftIO $ widgetQueueDraw drawingArea
    return True

  on drawingArea draw $ do
    w <- liftIO (fromIntegral <$> widgetGetAllocatedWidth drawingArea)
    h <- liftIO (fromIntegral <$> widgetGetAllocatedHeight drawingArea)
    translate (w / 2) (h / 2)
    game' <- liftIO $ readIORef game
    drawGame game'

  mainGUI

colorBlack   = setSourceRGB 0 0 0
colorTile    = setSourceRGB 0.969 0.922 0.82
colorBg      = setSourceRGB 0.29 0.514 0.831
colorDouble  = setSourceRGB 0.98 0.757 0.059
colorTripple = setSourceRGB 0.875 0.078 0.078
colorBis     = setSourceRGB 0.918 0.412 0.067

boardWidth = 600
tileWidth = boardWidth / fromIntegral boardSize

drawGame :: GameState -> Render ()
drawGame (GameState board _ players currentPlayer) = do
  drawBoard board
  translate (boardWidth / 2 - tileWidth / 2) (boardWidth / 2 - tileWidth / 2)
  mapM_ drawPlayer $ zip (map (\i -> (i, Just i == currentPlayer)) [0..]) $ toList players

drawBoard :: Board -> Render ()
drawBoard board = do
  translate (- boardWidth / 2) (- boardWidth / 2)
  let tw = tileWidth

  -- blue background
  colorBg
  rectangle 0 0 boardWidth boardWidth >> fill

  -- special cells
  let doubles = filter isDouble allCoords
  let tripples = filter isTripple allCoords
  let bises = filter isBis allCoords
  let specials = [(doubles, colorDouble), (tripples, colorTripple), (bises, colorBis)]
  forM_ specials $ \(coords, color) -> do
    color
    mapM_ (\(x, y) -> rectangle (fromIntegral x * tw) (fromIntegral y * tw) tw tw) coords
    fill

  -- grid
  mapM_ (\x -> line x 0 x boardWidth) [0,tw..boardWidth]
  mapM_ (\y -> line 0 y boardWidth y) [0,tw..boardWidth]
  colorBlack
  setLineWidth 2
  stroke

  -- tiles
  selectFontFace "Sans" FontSlantNormal FontWeightBold
  setFontSize 22
  mapM_ drawTile $ Map.assocs board

drawTile :: (Coord, Int) -> Render ()
drawTile ((x, y), t) = do
  let w = tileWidth / 2 - 2
  let (x', y') = (fromIntegral x * tileWidth + tileWidth / 2, fromIntegral y * tileWidth + tileWidth / 2)
  colorTile
  drawRoundedRect (x' - w) (x' + w) (y' - w) (y' + w) 4
  colorBlack
  drawText x' y' (show t)

drawPlayer :: ((Int, Bool), Player) -> Render ()
drawPlayer ((i, curr), (score, rack)) = do
  save
  rotate $ fromIntegral i * pi / 2
  translate 0 $ 3 * boardWidth / 5
  mapM_ drawTile $ zip (zip [-1, 0, 1] $ repeat 0) rack
  drawText (tileWidth * 5) (tileWidth / 2) $ show score
  when curr $ drawText (tileWidth * 4) (tileWidth / 2) "*"
  restore

line :: Double -> Double -> Double -> Double -> Render ()
line a b c d = moveTo a b >> lineTo c d

drawRoundedRect :: Double -> Double -> Double -> Double -> Double -> Render ()
drawRoundedRect left right top bottom radius = do
  let (a, b, c, d) = (left, right, top, bottom)
  newPath
  arc (a + radius) (c + radius) radius (2*(pi/2)) (3*(pi/2))
  arc (b - radius) (c + radius) radius (3*(pi/2)) (4*(pi/2))
  arc (b - radius) (d - radius) radius (0*(pi/2)) (1*(pi/2))
  arc (a + radius) (d - radius) radius (1*(pi/2)) (2*(pi/2))
  closePath
  fill

drawText :: Double -> Double -> String -> Render ()
drawText x y s = do
  e <- textExtents s
  let x' = x - (textExtentsWidth e / 2 + textExtentsXbearing e)
  let y' = y - (textExtentsHeight e / 2 + textExtentsYbearing e)
  moveTo x' y'
  showText s
