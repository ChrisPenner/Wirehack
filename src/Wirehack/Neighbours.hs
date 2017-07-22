{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
module Wirehack.Neighbours where

import Wirehack.Space

import Control.Comonad.Store
import Data.Functor.Rep

data Dir = L | R | U | D
  deriving (Show, Eq)

indOf :: Dir -> (Int, Int)
indOf U = (0, -1)
indOf D = (0, 1)
indOf L = (-1, 0)
indOf R = (1, 0)

addTup :: (Num x, Num y) => (x, y) -> (x, y) -> (x, y)
addTup (x, y) (offX, offY) = (x + offX, y + offY)

nearby :: ((Int, Int) ~ Rep r) => Dir -> ISpace r a -> a
nearby (indOf -> offsets) = peeks (addTup offsets)

move :: ((Int, Int) ~ Rep r) => Dir -> ISpace r a -> ISpace r a
move (indOf -> offsets) = seeks (addTup offsets)

