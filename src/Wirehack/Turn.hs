{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
module Wirehack.Turn where

import Wirehack.Space
import Wirehack.Components
import Wirehack.Neighbours
import Control.Comonad

data Status = Good | Bad | Neutral

validate :: ISpace D2 Component -> ISpace D2 Status
validate = extend check

-- pair :: Component -> Dir
-- pair PDown = U
-- pair PLeft = R
-- pair PUp = D
-- pair PRight = L
-- pair Empty = U

pair :: Dir -> Component
pair U = PDown
pair R = PLeft
pair D = PUp
pair L = PRight

check :: ISpace D2 Component -> Status
check w = case extract w of
            Empty -> Neutral
            Source -> Good
            PUp ->    if any match [R, L, D] then Good else Bad
            PDown ->  if any match [R, L, U] then Good else Bad
            PLeft ->  if any match [R, U, D] then Good else Bad
            PRight -> if any match [U, L, D] then Good else Bad
  where
    match d = nearby d w == pair d || nearby d w == Source
