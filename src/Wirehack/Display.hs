module Wirehack.Display where

import Wirehack.Components
import Wirehack.Space
import Wirehack.Index

import Data.Monoid
import Data.Maybe
import Data.Functor.Rep

import Control.Monad
import Control.Monad.State
import Control.Lens hiding (Index, Empty)

import Graphics.Vty as V

type Cell = Last Component

type HackM a = StateT (ISpace Row Col Component) IO a

forceComponent :: Space x y Cell -> Space x y Component
forceComponent = fmap (fromMaybe Empty . getLast)

showBoard :: Show a => ISpace x y a -> String
showBoard (ISpace _ (Space v)) = foldMap ((++ "\n") . foldMap show) v

start :: ISpace Row Col Component
start = tabulate (const Empty)

disp :: ISpace x y Component -> IO ()
disp = putStr . showBoard

interrupt :: V.Event
interrupt = EvKey (KChar 'c') [MCtrl]

startGame :: IO ()
startGame = flip evalStateT (tabulate (const Empty)) $
  liftIO (mkVty defaultConfig) >>= gameLoop

toggle :: (Index x, Index y) => ISpace x y Component -> ISpace x y Component
toggle = focus %~ toggleC
doMove :: (Index x, Index y) => V.Event -> ISpace x y Component -> ISpace x y Component
doMove (EvKey (KChar ' ') _) = toggle
doMove _ = id


gameLoop :: Vty -> HackM ()
gameLoop vty = do
  e <- liftIO $ nextEvent vty
  unless (e == interrupt) $ do
    get >>= liftIO . disp
    liftIO $ putStrLn "--------------"
    gameLoop vty
