module Wirehack.Neighbours where

import Wirehack.Space
import Wirehack.Index

import Control.Comonad.Store

data Dir = L | R | U | D
  deriving (Show, Eq)

indOf :: Dir -> (Int, Int)
indOf U = (0, (-1))
indOf D = (0, 1)
indOf L = ((-1), 0)
indOf R = (1, 0)

nearby :: (Index x, Index y, Monoid a) => Dir -> ISpace x y a -> a
nearby dir is@(ISpace foc s) =
  let l = addInd foc (indOf dir)
   in if foc == l
         then mempty
         else peek l is

