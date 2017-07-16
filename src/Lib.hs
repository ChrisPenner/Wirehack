{-# language DeriveFunctor #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Lib where

import Index

import Data.Vector
import Data.Monoid
import Data.Distributive
import Data.Functor.Rep
import Data.Typeable

import Control.Comonad

data Space x y a = Space (Ind x, Ind y) (Vector (Vector a))
  deriving (Eq, Functor)

instance (Show a, Typeable x, Typeable y) => Show (Space x y a) where
  show (Space foc v) = "Focus: " <> show foc <> ":\n" <> foldMap ((<> "\n"). show) v

instance (Index x, Index y) => Distributive (Space x y) where
  distribute fa = Space minBound (generate numCols genRow)
    where
      genRow y = generate numRows (genCell y)
      genCell y x = fmap (flip index (wrapI x, wrapI y)) fa
      numRows = unwrapI (maxBound :: Ind x) + 1
      numCols = unwrapI (maxBound :: Ind y) + 1

instance (Index x, Index y) => Representable (Space x y) where
  type Rep (Space x y) = (Ind x, Ind y)
  index (Space _ v) (unwrapI -> x, unwrapI -> y) = v ! y ! x
  tabulate desc = Space minBound $ generate numRows generator
    where
      numRows = unwrapI (maxBound :: Ind x) + 1
      numCols = unwrapI (maxBound :: Ind y) + 1
      generator x = generate numCols (\y -> desc (wrapI x, wrapI y))

instance (Index x, Index y) => Comonad (Space x y) where
  extract w@(Space f _) = index w f
  duplicate (Space foc v) =
    let Space _ v' = tabulate desc
     in Space foc v'
    where
      desc focus = Space focus v

moveTo :: (Index x, Index y) => (Ind x, Ind y) -> Space x y a -> Space x y a
moveTo foc (Space _ v) = Space foc v

moveBy :: (Index x, Index y) => (Ind x, Ind y) -> Space x y a -> Space x y a
moveBy offs (Space curr v)
  = Space (curr + offs) v

g  :: Space Row Col (Ind Row, Ind Col)
g = tabulate id
