{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language ViewPatterns #-}
{-# language TupleSections #-}
module Wirehack.Display where

import Wirehack.Components
import Wirehack.Space
import Wirehack.Neighbours
import Wirehack.Turn
import Wirehack.Dim

import Data.Functor.Rep
import qualified Data.Text as T

import Control.Applicative
import Control.Monad.State
import Control.Lens hiding (Index, Empty, index)
import Control.Exception
import Data.Monoid

import qualified Graphics.Vty as V

type HackM a = StateT (ISpace D2 Component) IO a

type RangeI r = (Rep r, Rep r)

board :: RangeI D2
board = ((0, 0), (60, 30))

start :: ISpace D2 Component
start = ISpace (0, 0) (tabulate (const Empty))

interrupt :: V.Event
interrupt = V.EvKey (V.KChar 'c') [V.MCtrl]

startGame :: IO ()
startGame = do
  vty <- V.mkVty V.defaultConfig
  handle (\(e :: SomeException) -> V.shutdown vty >> print e) $
    evalStateT (gameLoop vty) start

doMove :: V.Event -> HackM ()
doMove (V.EvKey V.KEnter _)      = focus .= Source
doMove (V.EvKey (V.KChar ' ') _) = focus .= Empty
doMove (V.EvKey (V.KChar 'H') _) = focus .= PLeft
doMove (V.EvKey (V.KChar 'J') _) = focus .= PDown
doMove (V.EvKey (V.KChar 'K') _) = focus .= PUp
doMove (V.EvKey (V.KChar 'L') _) = focus .= PRight
doMove (V.EvKey (V.KChar 'l') _) = modify $ move R
doMove (V.EvKey (V.KChar 'h') _) = modify $ move L
doMove (V.EvKey (V.KChar 'k') _) = modify $ move U
doMove (V.EvKey (V.KChar 'j') _) = modify $ move D
doMove _ = return ()

gameLoop :: V.Vty -> HackM ()
gameLoop vty = do
  st <- get
  let img = render . getRange board .  getSpace .  highlight . colorize . attrs $ st
  liftIO . V.update vty . V.picForImage $ img
  e <- liftIO $ V.nextEvent vty
  doMove e
  if e == interrupt
     then liftIO $ V.shutdown vty
     else gameLoop vty
  where
    highlight = focus . _1 <>~ highlighting
    highlighting  = V.withStyle V.currentAttr V.reverseVideo

render :: Show a => [[(V.Attr, a)]] -> V.Image
render = V.vertCat . fmap (V.horizCat . fmap rep)
  where
    rep (attr, T.pack . show -> txt) = V.text' attr txt

attrs :: Functor f => f a -> f (V.Attr, a)
attrs = fmap (V.defAttr,)

colorize :: ISpace D2 (V.Attr, Component) -> ISpace D2 (V.Attr, Component)
colorize w@(ISpace foc spc) = ISpace foc $ liftA2 combine (fmap color valid) spc
  where
    combine a (a', x) = (a' <> a, x)
    valid = getSpace . validate . fmap snd $ w
    color Good = V.withForeColor V.defAttr V.green
    color Bad = V.withForeColor V.defAttr V.red
    color Neutral = V.currentAttr




-- idISpace :: ISpace D2 (Rep D2)
-- idISpace = ISpace (0, 0) idD2

-- idD2 :: D2 (Rep D2)
-- idD2 = tabulate id

-- idSpace :: Space (Rep Space)
-- idSpace = tabulate id
