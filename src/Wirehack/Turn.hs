{-# language ScopedTypeVariables #-}
module Wirehack.Turn where

import Wirehack.Space
import Wirehack.Cell
import Wirehack.Neighbours
import Wirehack.Power
import Control.Lens hiding (Empty)

data Status = Good | Bad | Neutral

validate :: Bounds w h => ISpace w h Cell -> ISpace w h Status
validate = fmap toStatus . checkPower
  where
    toStatus True = Good
    toStatus _ = Bad

checkPower :: Bounds w h => ISpace w h Cell -> ISpace w h Bool
checkPower = fmap hasPower

stepPower :: forall w h. Bounds w h => ISpace w h Cell -> ISpace w h Cell
stepPower spc = set poweredBy <$> poweredByNeighbours <*> spc
  where
    poweredByNeighbours = getPoweredBy <$> spc <*> neighboursOf spc
