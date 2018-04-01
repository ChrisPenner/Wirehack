{-# language ViewPatterns #-}
module Wirehack.Neighbours where

import Wirehack.Space

import Control.Comonad.Store
import Data.Monoid

data Dir = L | R | U | D
  deriving (Show, Eq)

indOf :: Dir -> (Sum Int, Sum Int)
indOf U = (-1, 0)
indOf D = (1, 0)
indOf L = (0, -1)
indOf R = (0, 1)

nearby :: Bounds w h => Dir -> ISpace w h a -> a
nearby (indOf -> offsets) = peeks (<> offsets)

move :: Bounds w h => Dir -> ISpace w h a -> ISpace w h a
move (indOf -> offset) = seeks (<> offset)
