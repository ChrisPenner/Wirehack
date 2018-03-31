{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
module Wirehack.Neighbours where

import Wirehack.Space

import Control.Comonad.Store
import Data.Monoid

data Dir = L | R | U | D
  deriving (Show, Eq)

indOf :: Dir -> (Sum Int, Sum Int)
-- indOf :: (BaseOf (Rep s) ~ Sum Int, Representable s) => Dir -> (Sum Int, Sum Int)
indOf U = (-1, 0)
indOf D = (1, 0)
indOf L = (0, -1)
indOf R = (0, 1)

nearby :: Dir -> ISpace w h a -> a
nearby (indOf -> offsets) s@(ISpace _ _) = peeks (<> offsets) s

move :: Dir -> ISpace w h a -> ISpace w h a
move (indOf -> offsets) s@(ISpace _ _) = seeks (<> offsets) s
