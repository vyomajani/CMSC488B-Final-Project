{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)

import Game
import Solver
import FourByFourSolver
import NineByNineSolver

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg, bg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.), at)
import qualified Graphics.Vty as V

import Data.Maybe (fromMaybe)
import Data.List  (intersperse)

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- loadBoard $ Game { _board = initBoard,
                          _cursor = (0,0),
                          _solved = False,
                          _solution = Solver.solve initBoard }
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g -- Step Time
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g            -- Quit

handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ switchSize g

handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ move North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ move South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ move East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ move West g

handleEvent g (VtyEvent (V.EvKey V.KEnter []))      = continue $ showSolution g  

handleEvent g (VtyEvent (V.EvKey (V.KChar '1') [])) = continue $ register One g 
handleEvent g (VtyEvent (V.EvKey (V.KChar '2') [])) = continue $ register Two g
handleEvent g (VtyEvent (V.EvKey (V.KChar '3') [])) = continue $ register Three g
handleEvent g (VtyEvent (V.EvKey (V.KChar '4') [])) = continue $ register Four g
handleEvent g (VtyEvent (V.EvKey (V.KChar '5') [])) = continue $ register Five g
handleEvent g (VtyEvent (V.EvKey (V.KChar '6') [])) = continue $ register Six g
handleEvent g (VtyEvent (V.EvKey (V.KChar '7') [])) = continue $ register Seven g
handleEvent g (VtyEvent (V.EvKey (V.KChar '8') [])) = continue $ register Eight g
handleEvent g (VtyEvent (V.EvKey (V.KChar '9') [])) = continue $ register Nine g
handleEvent g (VtyEvent (V.EvKey V.KBS [])) = continue $ register Zero g

handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ showHint g

handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g = [vBox [ withBorderStyle BS.unicodeBold 
             $ B.borderWithLabel (str "Sudoku")
             $ drawGrid g
             , drawSolved g 
             , withBorderStyle BS.unicodeBold 
             $ B.borderWithLabel (str "Help")
             $ drawHelp g ]]

drawHelp :: Game -> Widget Name
drawHelp g = vBox [ str "Up / Down / Left / Right Arrow -> Move cursor",
                    str "0, 1, 2... -> Input number",
                    str "Enter -> Solution", 
                    str "H -> Hint",
                    str "S -> Switch to 4x4 or 9x9 board"]

drawSolved :: Game -> Widget Name 
drawSolved g 
  | g ^. solved = str "Solved!"
  | otherwise = emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = vBox rows
  where
    width = (if g ^. four then FourByFourSolver.width else NineByNineSolver.width)
    height = (if g ^. four then FourByFourSolver.height else NineByNineSolver.height)
    rows    = intersperse (str (if g ^. four then (replicate 8 '—') else (replicate 17 '—'))) [hBox $ cells x | x <- [0..width-1]]
    cells x = intersperse (str "|") [drawCell x y   | y <- [0..width-1]]
    drawCell x y =
      let f = if g ^. cursor == (x,y) then withAttr cursorAttr else id in
      f $ case g ^. board . at (x,y) of 
            Nothing -> str " "
            Just Zero -> str " "
            Just p -> str (Solver.valueConverter p)


theMap :: AttrMap
theMap = attrMap V.defAttr [(cursorAttr, bg V.red)]

cursorAttr :: AttrName 
cursorAttr = "cursorAttr"

