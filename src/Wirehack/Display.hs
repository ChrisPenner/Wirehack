{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language ViewPatterns #-}
module Wirehack.Display where

import Wirehack.Components
import Wirehack.Space
import Wirehack.Neighbours
import Wirehack.Turn

import Data.Functor.Rep
import qualified Data.Text as T

import Control.Applicative
import Control.Monad.State
import Control.Lens hiding (Index, Empty, index)
import Control.Exception
import Data.Monoid

import qualified Graphics.Vty as V

type HackM a = StateT (ISpace D2 Component) IO a

type Range r = (Rep r, Rep r)

board :: Range D2
board = ((0, 0), (10, 10))

-- getRange :: Representable r => r a -> [Rep r] -> [a]
-- getRange = fmap . index

getRange :: (Show a) => Range D2 -> D2 a -> [[a]]
getRange ((lowX, lowY), (highX, highY)) r =
  fmap (index r) <$> [[(x, y) | x <- [lowX..highX]] | y <- [lowY..highY]]

showRange :: Show a => [[a]] -> String
showRange = unlines . fmap (foldMap show)

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
doMove (V.EvKey (V.KChar ' ') _) = focus %= toggleC
doMove (V.EvKey (V.KChar 'l') _) = modify $ move R
doMove (V.EvKey (V.KChar 'h') _) = modify $ move L
doMove (V.EvKey (V.KChar 'k') _) = modify $ move U
doMove (V.EvKey (V.KChar 'j') _) = modify $ move D
doMove _ = return ()

gameLoop :: V.Vty -> HackM ()
gameLoop vty = do
  st <- get
  let img = render . getRange board . getSpace $ highlight st
  liftIO . V.update vty . V.picForImage $ img
  e <- liftIO $ V.nextEvent vty
  doMove e
  if e == interrupt
     then liftIO $ V.shutdown vty
     else gameLoop vty
  where
    highlight = (focus . _1 <>~ highlighting) . fmap ((,) V.defAttr)
    highlighting  = V.withStyle V.defAttr V.reverseVideo

render :: Show a => [[(V.Attr, a)]] -> V.Image
render = V.vertCat . fmap (V.horizCat . fmap rep)
  where
    rep (attr, T.pack . show -> txt) = V.text' attr txt

highlight :: ISpace D2 (V.Attr, Component) -> ISpace D2 (V.Attr, Component)
highlight w@(ISpace foc spc) = ISpace foc $ liftA2 combine (fmap color valid) spc
  where
    combine a (a', x) = (a <> a', x)
    valid :: D2 Bool
    valid = getSpace . validate . fmap snd $ w
    color True = V.withForeColor V.defAttr V.green
    color False = V.withForeColor V.defAttr V.red

