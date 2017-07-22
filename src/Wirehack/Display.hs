{-# language FlexibleInstances #-}
{-# language DeriveFunctor #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
module Wirehack.Display where

import Wirehack.Components
import Wirehack.Space
import Wirehack.Neighbours

import Data.Functor.Rep
import qualified Data.Text as T

import Control.Monad.State
import Control.Lens hiding (Index, Empty, index)
import Control.Exception

import qualified Graphics.Vty as V

data Style a = Highlight a | Default a
  deriving (Show, Eq, Functor)

type HackM a = StateT (ISpace D2 Component) IO a

type Range r = (Rep r, Rep r)

board :: Range D2
board = ((0, 0), (10, 10))

getRange :: (Show a) => Range D2 -> D2 a -> [[a]]
getRange ((lowX, lowY), (highX, highY)) r =
  fmap (index r) <$> [[(x, y) | x <- [lowX..highX]] | y <- [lowY..highY]]

showRange :: Show a => [[a]] -> String
showRange = unlines . fmap (foldMap show)

start :: ISpace D2 Component
start = ISpace (0, 0) (tabulate (const Empty))

disp :: ISpace D2 Component -> IO ()
disp = putStr . showRange . getRange board . getSpace

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
  liftIO . V.update vty . V.picForImage . render . getRange board . getSpace $ (highlight st)
  e <- liftIO $ V.nextEvent vty
  doMove e
  if e == interrupt
     then liftIO $ V.shutdown vty
     else gameLoop vty
  where
    highlight = (focus %~ toH) . fmap Default
    toH (Default s) = Highlight s
    toH x = x

class Renderable a where
  render :: a -> V.Image

instance Show a => Renderable (Style a) where
  render (Highlight s) = V.text' (V.withStyle V.defAttr V.reverseVideo) (T.pack $ show s)
  render (Default s) = V.text' V.defAttr (T.pack $ show s)

instance Renderable a => Renderable [[a]] where
  render = V.vertCat . fmap (V.horizCat . fmap render)

-- instance Renderable a => Renderable (ISpace D2 a) where
  -- render (ISpace _ spc) = fold $ fmap (V.horizCat . toList . fmap render) spc

-- getRange :: (Show a) => Range D2 -> D2 a -> [[a]]
-- getRange ((lowX, lowY), (highX, highY)) r = foldMap rowOf [lowX..highX]
--     where
--       rowOf x = foldMap (showInd x) [lowY..highY] <> "\n"
--       showInd x y = show $ index r (x, y)

