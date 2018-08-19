module Wirehack.Display
  ( renderSpace
  , stepGame
  ) where

import Wirehack.Render (render)
import Wirehack.State (space)
import Wirehack.Turn (stepPower)

import Control.Lens
import Eve (App)
import Eve.CLI (renderImage)

renderSpace :: App ()
renderSpace = do
  spc <- use space
  renderImage . render $ spc

stepGame :: App ()
stepGame = do
  space %= stepPower
  renderSpace
