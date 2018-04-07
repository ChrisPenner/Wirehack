{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language TemplateHaskell #-}
module Wirehack.State where

import Eve
import Control.Lens
import Wirehack.Space
import Wirehack.Cell
import Data.Default
import Wirehack.Neighbours
import Data.Functor.Rep

instance Bounds w h => Default (ISpace w h Cell) where
  def = ISpace (0, 0) (tabulate (const emp))

space :: (HasStates s) => Lens' s (ISpace 20 20 Cell)
space = stateLens

newtype Latch = Latch 
  { _latchVal :: Bool
  }

makeLenses ''Latch

instance Default Latch where
  def = Latch False

latch :: (HasStates s) => Lens' s Bool
latch = stateLens . latchVal

latchMove :: Dir -> App ()
latchMove d = do
  space . focus .= wire d
  space %= move d
