{-# language FlexibleInstances #-}
{-# language DeriveFunctor #-}
{-# language ScopedTypeVariables #-}
module Wirehack.Display where

import Wirehack.Components
import Wirehack.Space
import Wirehack.Index
import Wirehack.Neighbours

import Data.Foldable (toList, fold)
import Data.Functor.Rep
import qualified Data.Text as T

import Control.Monad.State
import Control.Lens hiding (Index, Empty)
import Control.Exception

import qualified Graphics.Vty as V

data Style a = Highlight a | Default a
  deriving (Show, Eq, Functor)

type HackM a = StateT (ISpace Col Row Component) IO a

showBoard :: Show a => ISpace x y a -> String
showBoard (ISpace _ (Space v)) = foldMap ((++ "\n") . foldMap show) v

start :: ISpace Col Row Component
start = tabulate (const Empty)

disp :: ISpace x y Component -> IO ()
disp = putStr . showBoard

interrupt :: V.Event
interrupt = V.EvKey (V.KChar 'c') [V.MCtrl]

startGame :: IO ()
startGame = do
  vty <- V.mkVty V.defaultConfig
  handle (\(e :: SomeException) -> V.shutdown vty >> print e) $
    evalStateT (gameLoop vty) (tabulate (const Empty))

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
  liftIO . V.update vty . V.picForImage $ render (highlight st)
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

instance Renderable a => Renderable (ISpace x y a) where
  render (ISpace _ (Space v)) = fold $ fmap (V.horizCat . toList . fmap render) v
