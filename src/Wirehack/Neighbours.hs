{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
module Wirehack.Neighbours where

import Wirehack.Space

import Control.Comonad.Store
import Data.Monoid
import Data.Functor.Rep

data Dir = L | R | U | D
  deriving (Show, Eq)

indOf :: Dir -> (Sum Int, Sum Int)
indOf U = (0, -1)
indOf D = (0, 1)
indOf L = (-1, 0)
indOf R = (1, 0)

nearby :: ((Sum Int, Sum Int) ~ Rep r) => Dir -> ISpace r a -> a
nearby (indOf -> offsets) = peeks (mappend offsets)

move :: ((Sum Int, Sum Int) ~ Rep r) => Dir -> ISpace r a -> ISpace r a
move (indOf -> offsets) = seeks (mappend offsets)

