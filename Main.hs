{-# LANGUAGE DeriveDataTypeable #-}

import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Control.Monad.Trans (liftIO)

import Control.Monad
import Data.IORef
import Data.Foldable
import System.Random
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq
import System.Console.CmdArgs hiding ((:=))

import Game
import Utils

playAI :: GameState -> (GameState, Int)
playAI gs@(GameState _ _ _ Nothing _) = (gs, -2)
playAI gs@(GameState board _ players (Just currentPlayer) _) = gs'
  where
    gs' = if not (null currLegalMoves) then playMove gs bestMove else (playChangeAll gs, -1)
    currLegalMoves = legalMoves board $ snd $ Seq.index players currentPlayer
    bestMove = maximumOn (evaluate board) currLegalMoves

evaluate :: Board -> Move -> Int
evaluate board move = scoreFor board move + if any isBis $ map fst move then 30 else 0

data Options = Options { numberPlayers :: Int } deriving (Data, Typeable)

defOptions =
  Options { numberPlayers = 4 &= typ "INT" }
    &= program "triolet"
    &= summary "Implementation of the Triolet board game"

main = do
  options <- cmdArgs defOptions
  unless (numberPlayers options `elem` [2..4]) $ fail "the number of players must be in [2..4]"

  -- setStdGen $ mkStdGen 4
  game <- newStdGen >>= (newIORef . initGame (numberPlayers options))

  initGUI

  drawingArea <- drawingAreaNew
  statusbar <- statusbarNew

  vb <- vBoxNew False 0
  boxPackStart vb drawingArea PackGrow 0
  boxPackEnd vb statusbar PackNatural 0

  window <- windowNew
  set window [windowTitle := "Triolet" ]
  on window deleteEvent $ liftIO mainQuit >> return False
  containerAdd window vb
  widgetShowAll window

  on drawingArea buttonPressEvent $ do
    gameState <- liftIO $ readIORef game
    let (gameState', points) = playAI gameState
    let msg = case points of
          -2 -> "Game is over."
          -1 -> "Player changed all his tiles."
          _  -> "Player scored " ++ show points ++ " points."
    liftIO $ writeIORef game gameState'
    liftIO $ statusbarPush statusbar 0 msg
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
colorRack    = setSourceRGB 0.5 0.5 0.5
colorDouble  = setSourceRGB 0.98 0.757 0.059
colorTripple = setSourceRGB 0.875 0.078 0.078
colorBis     = setSourceRGB 0.918 0.412 0.067

boardWidth = 600
tileWidth = boardWidth / fromIntegral boardSize

drawGame :: GameState -> Render ()
drawGame (GameState board _ players currentPlayer _) = do
  selectFontFace "Sans" FontSlantNormal FontWeightBold
  setFontSize 22

  drawBoard board
  mapM_ drawPlayer $ zip (map (\i -> (i, Just i == currentPlayer)) [0..]) $ toList players

drawBoard :: Board -> Render ()
drawBoard board = do
  save
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
  mapM_ drawTile $ Map.toList board

  restore

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
  translate (- tileWidth / 2) $ 3 * boardWidth / 5
  rectangle (- tileWidth) 0 (tileWidth * 3) tileWidth >> colorRack >> fill
  mapM_ drawTile $ zip (zip [-1, 0, 1] $ repeat 0) rack
  colorBlack
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
