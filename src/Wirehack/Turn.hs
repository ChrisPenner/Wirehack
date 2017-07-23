{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
module Wirehack.Turn where

import Wirehack.Space
import Wirehack.Components
import Wirehack.Neighbours
import Control.Comonad

data Status = Good | Bad | Neutral

validate :: ISpace D2 Component -> ISpace D2 Status
validate = extend go
  where
    go w = case extract w of
             Up -> if nearby U w == Down then Good else Bad
             Down -> if nearby D w == Up then Good else Bad
             Empty -> Neutral
