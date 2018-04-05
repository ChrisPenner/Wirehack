{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
module Wirehack.Display where

import Wirehack.Components
import Wirehack.Space
import Wirehack.Neighbours
import Wirehack.Turn
import Wirehack.Render

import Eve
import Eve.CLI

import Control.Concurrent
import Data.Vector (Vector)
import Data.Functor.Rep
import qualified Data.Text as T

import Control.Arrow ((&&&))
import Control.Applicative
import Control.Monad.State
import Control.Lens hiding (Index, Empty, index)
import Control.Exception
import Data.Monoid
import Data.Default

import qualified Graphics.Vty as V

type HackM w h a = StateT (ISpace w h Cell) IO a

instance Default (ISpace 20 20 Cell) where
  def = ISpace (0, 0) (tabulate (const emp))

interrupt :: Keypress
interrupt = Keypress (V.KChar 'c') [V.MCtrl]

space :: (HasStates s) => Lens' s (ISpace 20 20 Cell)
space = stateLens

doMove :: Keypress -> App ()
doMove (Keypress V.KEnter _)      = space . focus .= source
doMove (Keypress (V.KChar ' ') _) = space . focus .= emp
doMove (Keypress (V.KChar 'H') _) = space . focus .= pl
doMove (Keypress (V.KChar 'J') _) = space . focus .= pd
doMove (Keypress (V.KChar 'K') _) = space . focus .= pu
doMove (Keypress (V.KChar 'L') _) = space . focus .= pr
doMove (Keypress (V.KChar 'l') _) = space %= move R
doMove (Keypress (V.KChar 'h') _) = space %= move L
doMove (Keypress (V.KChar 'k') _) = space %= move U
doMove (Keypress (V.KChar 'j') _) = space %= move D
doMove _ = return ()

handleKeypress :: Keypress -> App ()
handleKeypress k | k == interrupt = exit
handleKeypress k = doMove k

renderSpace :: App ()
renderSpace = do
  spc <- use space
  renderImage . render $ spc

stepGame :: App ()
stepGame = do
  space %= stepPower
  renderSpace
