module Wirehack.Neighbours where

import Wirehack.Space
import Wirehack.Index

import Control.Comonad.Store (pos, peek, seeks)

data Dir = L | R | U | D
  deriving (Show, Eq)

indOf :: Dir -> (Int, Int)
indOf U = (0, -1)
indOf D = (0, 1)
indOf L = (-1, 0)
indOf R = (1, 0)

nearby :: (Index x, Index y, Monoid a) => Dir -> ISpace x y a -> a
nearby dir spc =
  let l = addInd (pos spc) (indOf dir)
   in if pos spc == l
         then mempty
         else peek l spc

move :: (Index x, Index y) => Dir -> ISpace x y a -> ISpace x y a
move dir = seeks (flip addInd (indOf dir))

