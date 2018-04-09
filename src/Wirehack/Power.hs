module Wirehack.Power where

import Wirehack.Cell
import Wirehack.Neighbours
import Data.Functor.Rep
import Data.Monoid
import Data.Foldable

getPoweredBy :: Cell -> Neighbours Cell -> Neighbours Bool
getPoweredBy Cell{_component=Source} _ = tabulate (const False)
getPoweredBy _ neighbours = imapRep (\i cell -> powers (flipDir i) cell) neighbours

hasPower :: Cell -> Bool
hasPower Cell{_component=Source} = True
hasPower Cell{_component=And, _poweredBy=p} = (>= 2) . length . filter id $ toList p
hasPower Cell{_poweredBy=p} = getAny $ foldMap Any p

powers :: Dir -> Cell -> Bool
powers _ Cell{_component=Source} = True
powers _ Cell{_component=Sink, _poweredBy=p} = getAny $ foldMap Any p
powers _ Cell{_component=And, _poweredBy=p} = (>= 2) . length . filter id $ toList p
powers x Cell{_component=Cross, _poweredBy=p} = p `index` flipDir x
powers x Cell{_component=Wire x', _poweredBy=p} 
  | (getAny $ foldMap Any p) = x == x'
powers _ _ = False
