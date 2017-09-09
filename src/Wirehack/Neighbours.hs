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
-- indOf :: (BaseOf (Rep s) ~ Sum Int, Representable s) => Dir -> (Sum Int, Sum Int)
indOf U = (-1, 0)
indOf D = (1, 0)
indOf L = (0, -1)
indOf R = (0, 1)

nearby :: ((Sum Int, Sum Int) ~ Rep r) => Dir -> ISpace r a -> a
nearby (indOf -> offsets) = peeks (mappend offsets)

move :: ((Sum Int, Sum Int) ~ Rep r) => Dir -> ISpace r a -> ISpace r a
move (indOf -> offsets) = seeks (mappend offsets)

