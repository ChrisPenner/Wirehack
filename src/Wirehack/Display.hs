{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
{-# language DataKinds #-}
module Wirehack.Display where

import Wirehack.Components
import Wirehack.Space
import Wirehack.Neighbours
import Wirehack.Turn
import Wirehack.Render

import Data.Vector (Vector)
import Data.Functor.Rep
import qualified Data.Text as T

import Control.Arrow ((&&&))
import Control.Applicative
import Control.Monad.State
import Control.Lens hiding (Index, Empty, index)
import Control.Exception
import Data.Monoid

import qualified Graphics.Vty as V

type HackM w h a = StateT (ISpace w h Cell) IO a

start :: ISpace 20 20 Cell
start = ISpace (0, 0) (tabulate (const emp))

interrupt :: V.Event
interrupt = V.EvKey (V.KChar 'c') [V.MCtrl]

startGame :: IO ()
startGame = do
  vty <- V.mkVty V.defaultConfig
  handle (\(e :: SomeException) -> V.shutdown vty >> print e) $
    evalStateT (gameLoop vty) start

doMove :: Bounds w h => V.Event -> HackM w h ()
doMove (V.EvKey V.KEnter _)      = focus .= source
doMove (V.EvKey (V.KChar ' ') _) = focus .= emp
doMove (V.EvKey (V.KChar 'H') _) = focus .= pl
doMove (V.EvKey (V.KChar 'J') _) = focus .= pd
doMove (V.EvKey (V.KChar 'K') _) = focus .= pu
doMove (V.EvKey (V.KChar 'L') _) = focus .= pr
doMove (V.EvKey (V.KChar 'l') _) = modify $ move R
doMove (V.EvKey (V.KChar 'h') _) = modify $ move L
doMove (V.EvKey (V.KChar 'k') _) = modify $ move U
doMove (V.EvKey (V.KChar 'j') _) = modify $ move D
doMove _ = return ()

gameLoop :: Bounds w h => V.Vty -> HackM w h ()
gameLoop vty = do
  spc <- get
  liftIO . V.update vty . V.picForImage . render $ spc
  e <- liftIO $ V.nextEvent vty
  doMove e
  if e == interrupt
     then liftIO $ V.shutdown vty
     else stepGame *> gameLoop vty

stepGame :: Bounds w h => HackM w h ()
stepGame = do
  modify stepPower
