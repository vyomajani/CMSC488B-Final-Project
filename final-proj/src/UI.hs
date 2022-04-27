{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)

import Game
import Solver

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
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g -- Step Time
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g            -- Quit
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ move North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ move South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ move East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ move West g
-- handleEvent g (VtyEvent (V.EvKey V.KEnter []))      = continue $ register g -- Enter Key should test board if full, or give hint if not full? 
-- handleEvent g (VtyEvent (V.EvKey (V.KChar '1') [])) = continue $ turn North g
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g = [ withBorderStyle BS.unicodeBold 
             $ B.borderWithLabel (str "Sudoku")
             $ drawGrid g ]

-- drawGameOver :: Game -> Widget Name
-- drawGameOver g
--   | g ^. done = str "Game Over!"
--   | otherwise = emptyWidget
{-
  let children = map (\g' -> (minimax g' (g ^. player)), g' ^. cursor) (moves g (g ^. player)) in
  str (show (snd (maximum children)))
-}

drawGrid :: Game -> Widget Name
drawGrid g = vBox rows
  where
    rows    = intersperse (str (replicate 17 '—')) [hBox $ cells y | y <- [0..height-1]]
    cells y = intersperse (str "|") [drawCell x y   | x <- [0..width-1]]
    drawCell x y =
      let f = if g ^. cursor == (x,y) then withAttr cursorAttr else id in
      f $ case g ^. board . at (x,y) of
            Nothing -> str " "
            Just p  -> str (show p)

theMap :: AttrMap
theMap = attrMap V.defAttr [(cursorAttr, bg V.red)]

cursorAttr :: AttrName 
cursorAttr = "cursorAttr"

