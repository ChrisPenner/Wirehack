{-# LANGUAGE ScopedTypeVariables #-}

module Wirehack.Turn
  ( stepPower
  ) where

import Control.Lens hiding (Empty)
import Wirehack.Cell (Cell(..), poweredBy)
import Wirehack.Neighbours (neighboursOf)
import Wirehack.Power (getPoweredBy)
import Wirehack.Space (Bounds, ISpace(..))

stepPower ::
     forall w h. Bounds w h
  => ISpace w h Cell
  -> ISpace w h Cell
stepPower spc = set poweredBy <$> poweredByNeighbours <*> spc
  where
    poweredByNeighbours = getPoweredBy <$> spc <*> neighboursOf spc
