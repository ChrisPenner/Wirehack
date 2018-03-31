module Wirehack.Turn where

import Wirehack.Space
import Wirehack.Components
import Wirehack.Neighbours
import Control.Comonad

data Status = Good | Bad | Neutral

validate :: Bounds w h => ISpace w h Component -> ISpace w h Status
validate = extend check

pair :: Dir -> Component
pair U = PDown
pair R = PLeft
pair D = PUp
pair L = PRight

check :: Bounds w h => ISpace w h Component -> Status
check w = case extract w of
            Empty -> Neutral
            Source -> Good
            PUp ->    if any match [R, L, D] then Good else Bad
            PDown ->  if any match [R, L, U] then Good else Bad
            PLeft ->  if any match [R, U, D] then Good else Bad
            PRight -> if any match [U, L, D] then Good else Bad
  where
    match d = nearby d w == pair d || nearby d w == Source
