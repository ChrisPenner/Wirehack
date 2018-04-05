{-# language TemplateHaskell #-}
{-# language RankNTypes #-}
module Wirehack.Loop where

import Eve
import Control.Lens (makeLenses)
import Data.Default (Default(..))
import System.IO
import Control.Concurrent
import Control.Monad

data GameState = GameState
makeLenses ''GameState

instance Default GameState where
  def = GameState
