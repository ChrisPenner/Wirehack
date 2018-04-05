module Wirehack.Power where

import Wirehack.Cell
import Wirehack.Neighbours
import Data.Functor.Rep
import Data.Monoid

getPoweredBy :: Cell -> Neighbours Cell -> Neighbours Bool
getPoweredBy Cell{_component=Source} _ = tabulate (const False)
getPoweredBy _ neighbours = imapRep (\i cell -> powers (flipDir i) cell) neighbours

hasPower :: Cell -> Bool
hasPower Cell{_component=Source} = True
hasPower Cell{_poweredBy=p} = getAny $ foldMap Any p

powers :: Dir -> Cell -> Bool
powers _ Cell{_component=Source} = True
powers x Cell{_component=Cross, _poweredBy=p} = p `index` flipDir x
powers U Cell{_component=PUp, _poweredBy=p} = getAny $ foldMap Any p
powers D Cell{_component=PDown, _poweredBy=p} = getAny $ foldMap Any p
powers L Cell{_component=PLeft, _poweredBy=p} = getAny $ foldMap Any p
powers R Cell{_component=PRight, _poweredBy=p} = getAny $ foldMap Any p
powers _ _ = False

