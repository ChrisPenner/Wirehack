module Wirehack.Display where

import Wirehack.Space
import Wirehack.Events
import Wirehack.Render
import Wirehack.Turn
import Wirehack.State

import Eve
import Eve.CLI
import Control.Lens

renderSpace :: App ()
renderSpace = do
  spc <- use space
  renderImage . render $ spc

stepGame :: App ()
stepGame = do
  space %= stepPower
  renderSpace

