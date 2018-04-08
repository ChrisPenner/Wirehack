{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
module Wirehack.Marshal where

import Wirehack.Cell
import Wirehack.Space
import Wirehack.Neighbours
import Data.Functor.Rep
import qualified Data.Text as T
import Safe


fromText :: forall w h. (Bounds w h) => T.Text -> Maybe (Space w h Cell)
fromText txt = sequenceA $ tabulate (fmap cellFromChar . buildSpace)
  where
    cellFromChar :: Char -> Cell
    cellFromChar '.' = emp
    cellFromChar '*' = source
    cellFromChar 'o' = sink
    cellFromChar '>' = wire R
    cellFromChar '<' = wire L
    cellFromChar '^' = wire U
    cellFromChar 'v' = wire D
    cellFromChar _ = emp
    buildSpace ::  Ind w h -> Maybe Char
    buildSpace (Mod x, Mod y) = do
      line <- lines' `atMay` y 
      if T.length line <= x 
         then Nothing
         else Just (line `T.index` x)
    lines' = T.lines txt

