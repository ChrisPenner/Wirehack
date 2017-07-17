{-# language DeriveFunctor #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Wirehack.Space where

import Wirehack.Index

import Data.Vector as V ((!), generate, Vector, fromList, zipWith)
import Data.Monoid
import Data.Distributive
import Data.Functor.Rep
import Data.Typeable

import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Store

import Control.Lens hiding (Index, index)
import qualified Control.Lens.At as I

type instance I.Index (Space x y a) = Rep (Space x y)
type instance I.IxValue (Space x y a) = a

instance (Index x, Index y) => I.Ixed (Space x y a) where
  ix ind@(x, y) = lens getter setter
    where
      getter s = index s ind
      setter (Space v) new = Space (v & ix (unwrapI y) . ix (unwrapI x) .~ new)

focus :: (Index x, Index y) => Lens' (ISpace x y a) a
focus = lens getter setter
  where
    getter = extract
    setter (ISpace foc s) new = ISpace foc (s & ix foc .~ new)

data ISpace x y a = ISpace (x, y) (Space x y a)
  deriving (Eq, Functor)

instance (Show a, Show x, Show y, Typeable x, Typeable y) => Show (ISpace x y a) where
  show (ISpace foc (Space v)) = "Foc: " ++ show foc ++ "\n" ++ foldMap ((<> "\n"). show) v

newtype Space x y a = Space (Vector (Vector a))
  deriving (Eq, Functor)

instance Foldable (Space x y) where
  foldMap f (Space v) = foldMap (foldMap f) v

instance (Show a, Typeable x, Typeable y) => Show (Space x y a) where
  show (Space v) = foldMap ((<> "\n"). show) v

instance (Index x, Index y) => Distributive (Space x y) where
  distribute = distributeRep

instance (Index x, Index y) => Representable (Space x y) where
  type Rep (Space x y) = (x, y)
  index (Space v) (unwrapI -> x, unwrapI -> y) = v ! y ! x
  tabulate desc = Space $ generate numRows generator
    where
      numCols = unwrapI (maxBound :: x) + 1
      numRows = unwrapI (maxBound :: y) + 1
      generator y = generate numCols (\x -> desc (wrapI x, wrapI y))

instance (Index x, Index y) => Distributive (ISpace x y) where
  distribute = distributeRep

instance (Index x, Index y) => Representable (ISpace x y) where
  type Rep (ISpace x y) = (x, y)
  index (ISpace _ s) = index s
  tabulate desc = ISpace minBound (tabulate desc)

instance (Index x, Index y) => Comonad (ISpace x y) where
  extract (ISpace ind s) = index s ind
  duplicate (ISpace foc v) = ISpace foc $ tabulate desc
    where
      desc fc = ISpace fc v

instance (Index x, Index y) => ComonadEnv (x, y) (ISpace x y) where
  ask = pos

instance (Index x, Index y) => ComonadStore (x, y) (ISpace x y) where
  pos (ISpace foc _) = foc
  peek = flip index

instance (Monoid a, Index x, Index y) => Monoid (Space x y a) where
  mempty = tabulate (const mempty)
  Space v `mappend` Space v' = Space (V.zipWith (V.zipWith mappend) v v')

moveBy :: (Index x, Index y) => (x, y) -> ISpace x y a -> ISpace x y a
moveBy (xOff, yOff) = seeks adjust
  where
    adjust (x, y) = (x + xOff, y + yOff)

fromLists :: (Index x, Index y) => [[a]] -> Space x y a
fromLists xs = Space $ generate (length xs) rows
  where
    rows i = fromList (xs !! i)

g  :: ISpace Col Row  (Col, Row)
g = tabulate id
