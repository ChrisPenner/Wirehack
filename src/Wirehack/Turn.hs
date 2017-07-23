{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
module Wirehack.Turn where

import Wirehack.Space
import Wirehack.Components
import Wirehack.Neighbours
import Control.Comonad

validate :: ISpace D2 Component -> ISpace D2 Bool
validate = extend go
  where
    go spc =
      case extract spc of
        Up -> nearby U spc == Down
        Down -> nearby D spc == Up
        Empty -> True
