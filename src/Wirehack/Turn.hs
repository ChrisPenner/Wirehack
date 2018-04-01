module Wirehack.Turn where

import Wirehack.Space
import Wirehack.Components
import Wirehack.Neighbours
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
checkPower = extend isPowered
  where
    isPowered :: Bounds w h => ISpace w h Cell -> Bool
    isPowered spc | spc ^. focus . component == Source = True
    isPowered spc = getAny . foldMap Any . imapRep powersMe . neighbourCells $ spc
    powersMe :: Dir -> Cell -> Bool
    powersMe = powers . flipDir
    neighbourCells :: Bounds w h => ISpace w h Cell -> Neighbours Cell
    neighbourCells = experiment getNeighbours
    getNeighbours :: Ind -> Neighbours Ind
    getNeighbours ind = mappend ind <$> neighbourPos

stepPower :: Bounds w h => ISpace w h Cell -> ISpace w h Cell
stepPower spc = set powered <$> checkPower spc <*> spc
