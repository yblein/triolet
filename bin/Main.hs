{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Control.Monad.Trans (liftIO)

import Control.Monad
import Data.IORef
import Data.Foldable
import System.Random
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq
import System.Console.CmdArgs hiding ((:=), name)

import Game
import Utils
import Eval

playAI :: GameState -> Player -> (GameState, Int)
playAI gs@(GameState { board, bag }) player = gs'
  where
    gs'
      | not (null currLegalMoves) = playMove gs $ bestMove
      | length bag >= 3 = (playChangeAll gs, -1)
      | otherwise = (playPass gs, -2)
    currLegalMoves = legalMoves board $ rack player
    bestMove = maximumOn (evaluate board) currLegalMoves

data Options = Options { numberPlayers :: Int } deriving (Data, Typeable)

defOptions =
  Options { numberPlayers = 4 &= typ "INT" }
    &= program "triolet"
    &= summary "Implementation of the Triolet board game"

main = do
  options <- cmdArgs defOptions
  unless (numberPlayers options `elem` [2..4]) $ fail "the number of players must be in [2..4]"

  -- setStdGen $ mkStdGen 4
  let names = map (\i -> "Player " ++ show i) [1..numberPlayers options]
  game <- newStdGen >>= (newIORef . initGame names)

  initGUI

  drawingArea <- drawingAreaNew
  statusbar <- statusbarNew

  vb <- vBoxNew False 0
  boxPackStart vb drawingArea PackGrow 0
  boxPackEnd vb statusbar PackNatural 0

  window <- windowNew
  set window [windowTitle := "Triolet" ]
  containerAdd window vb
  widgetShowAll window

  on window deleteEvent $ liftIO mainQuit >> return False

  on window keyPressEvent $ do
    gameState <- liftIO $ readIORef game

    msg <- case currentPlayer gameState of
      Nothing -> return "Game is over."
      Just currentPlayer -> do
        let player = Seq.index (players gameState) currentPlayer
        let (gameState', points) = playAI gameState player
        let msg = name player ++ case points of
              -2 -> " cannot play, skip turn"
              -1 -> " changed all his tiles."
              _  -> " scored " ++ show points ++ " points."
        liftIO $ writeIORef game gameState'
        liftIO $ widgetQueueDraw drawingArea
        return msg

    liftIO $ statusbarPush statusbar 0 msg
    return True

  on drawingArea draw $ do
    w <- liftIO (fromIntegral <$> widgetGetAllocatedWidth drawingArea)
    h <- liftIO (fromIntegral <$> widgetGetAllocatedHeight drawingArea)
    translate (w / 2) (h / 2)
    let ratio = (min w h) / 900
    scale ratio ratio
    game' <- liftIO $ readIORef game
    drawGame game'

  mainGUI

setColorBlack   = setSourceRGB 0 0 0
setColorTile    = setSourceRGB 0.969 0.922 0.82
setColorBoard   = setSourceRGB 0.29 0.514 0.831
setColorRack    = setSourceRGB 0.5 0.5 0.5
setColorDouble  = setSourceRGB 0.98 0.757 0.059
setColorTripple = setSourceRGB 0.875 0.078 0.078
setColorBis     = setSourceRGB 0.918 0.412 0.067

boardWidth = 600
tileWidth = boardWidth / fromIntegral boardSize

jokerChar = 'χ' -- '✯'

drawGame :: GameState -> Render ()
drawGame gs = do
  selectFontFace "Sans" FontSlantNormal FontWeightBold
  setFontSize 22

  drawBoard (board gs)
  mapM_ drawPlayer $ zip (map (\i -> (i, Just i == currentPlayer gs)) [0..]) $ toList (players gs)

drawBoard :: Board -> Render ()
drawBoard board = withLocalState $ do
  translate (- boardWidth / 2) (- boardWidth / 2)
  let tw = tileWidth

  -- blue background
  setColorBoard
  rectangle 0 0 boardWidth boardWidth
  fill

  -- special cells
  let doubles = filter isDouble allCoords
  let tripples = filter isTripple allCoords
  let bises = filter isBis allCoords
  let specials = [(doubles, setColorDouble), (tripples, setColorTripple), (bises, setColorBis)]
  forM_ specials $ \(coords, setColor) -> do
    setColor
    mapM_ (\(x, y) -> rectangle (fromIntegral x * tw) (fromIntegral y * tw) tw tw) coords
    fill

  -- grid
  mapM_ (\x -> line x 0 x boardWidth) [0,tw..boardWidth]
  mapM_ (\y -> line 0 y boardWidth y) [0,tw..boardWidth]
  setColorBlack
  setLineWidth 2
  stroke

  -- tiles
  mapM_ drawTile $ Map.toList board

drawTile :: (Coord, Int) -> Render ()
drawTile ((x, y), t) = do
  let w = tileWidth / 2 - 2
  let (x', y') = (fromIntegral x * tileWidth + tileWidth / 2, fromIntegral y * tileWidth + tileWidth / 2)
  setColorTile
  drawRoundedRect (x' - w) (x' + w) (y' - w) (y' + w) 4
  setColorBlack
  drawText x' y' $ if isJoker t then [jokerChar] else show t

drawPlayer :: ((Int, Bool), Player) -> Render ()
drawPlayer ((i, curr), player) = withLocalState $ do
  rotate $ fromIntegral i * pi / 2
  translate (- tileWidth / 2) $ 3 * boardWidth / 5

  -- rack background
  rectangle (- tileWidth) 0 (tileWidth * 3) tileWidth
  setColorRack
  fill

  -- rack tiles
  mapM_ drawTile $ zip (zip [-1, 0, 1] $ repeat 0) $ rack player

  -- score and turn indicator
  setColorBlack
  drawText (tileWidth * 5) (tileWidth / 2) $ show $ score player
  when curr $ drawText (tileWidth * 4) (tileWidth / 2) "*"

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

withLocalState :: Render () -> Render ()
withLocalState f = save >> f >> restore
