{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
{-# language DataKinds #-}
module Wirehack.Display where

import Wirehack.Components
import Wirehack.Space
import Wirehack.Neighbours
import Wirehack.Turn

import Data.Vector (Vector)
import Data.Functor.Rep
import qualified Data.Text as T

import Control.Applicative
import Control.Monad.State
import Control.Lens hiding (Index, Empty, index)
import Control.Exception
import Data.Monoid

import qualified Graphics.Vty as V

type HackM w h a = StateT (ISpace w h Component) IO a

start :: ISpace 20 20 Component
start = ISpace (0, 0) (tabulate (const Empty))

interrupt :: V.Event
interrupt = V.EvKey (V.KChar 'c') [V.MCtrl]

startGame :: IO ()
startGame = do
  vty <- V.mkVty V.defaultConfig
  handle (\(e :: SomeException) -> V.shutdown vty >> print e) $
    evalStateT (gameLoop vty) start

doMove :: Bounds w h => V.Event -> HackM w h ()
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

gameLoop :: Bounds w h => V.Vty -> HackM w h ()
gameLoop vty = do
  st <- get
  let img = render . highlight . colorize . attrs $ st
  liftIO . V.update vty . V.picForImage $ img
  e <- liftIO $ V.nextEvent vty
  doMove e
  if e == interrupt
     then liftIO $ V.shutdown vty
     else gameLoop vty
  where
    highlight = focus . _1 <>~ highlighting
    highlighting  = V.withStyle V.currentAttr V.reverseVideo

render :: Show a => ISpace w h (V.Attr, a) -> V.Image
render (ISpace _ (Space spc)) =
  foldr (V.vertJoin . foldInner) V.emptyImage $  spc
  where
    foldInner :: Show a => Vector (V.Attr, a) -> V.Image
    foldInner = foldr (V.horizJoin . rep) V.emptyImage
    rep :: Show a => (V.Attr, a) -> V.Image
    rep (attr, T.pack . show -> txt) = V.text' attr txt

attrs :: Functor f => f a -> f (V.Attr, a)
attrs = fmap ((,) V.defAttr)

colorize :: Bounds w h => ISpace w h (V.Attr, Component) -> ISpace w h (V.Attr, Component)
colorize spc = liftA2 combine (color <$> valid) spc
  where
    combine a (a', x) = (a' <> a, x)
    valid = validate . fmap snd $ spc
    color Good = V.withForeColor V.defAttr V.green
    color Bad = V.withForeColor V.defAttr V.red
    color Neutral = V.currentAttr
