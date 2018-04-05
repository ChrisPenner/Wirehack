{-# language ScopedTypeVariables #-}
module Wirehack.Turn where

import Wirehack.Space
import Wirehack.Cell
import Wirehack.Neighbours
import Wirehack.Power
import Control.Comonad
import Data.Functor.Rep
import Data.Monoid
import Control.Comonad.Representable.Store
import Control.Lens hiding (Empty)

data Status = Good | Bad | Neutral

validate :: Bounds w h => ISpace w h Cell -> ISpace w h Status
validate = fmap toStatus . checkPower
  where
    toStatus True = Good
    toStatus _ = Bad

pair :: Dir -> Component
pair U = PDown
pair R = PLeft
pair D = PUp
pair L = PRight

checkPower :: Bounds w h => ISpace w h Cell -> ISpace w h Bool
checkPower = fmap hasPower

stepPower :: forall w h. Bounds w h => ISpace w h Cell -> ISpace w h Cell
stepPower spc = set poweredBy <$> poweredByNeighbours <*> spc
  where
    poweredByNeighbours = getPoweredBy <$> spc <*> cellNeighbours
    cellNeighbours :: ISpace w h (Neighbours Cell)
    cellNeighbours = extend (experiment (\i -> fmap (<> i) neighbourPos)) spc
