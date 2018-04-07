{-# language ViewPatterns #-}
{-# language DeriveFunctor #-}
{-# language TypeFamilies #-}
{-# language RecordWildCards #-}
module Wirehack.Neighbours where

import Data.Functor.Rep
import Data.Distributive

import Wirehack.Space

import Control.Comonad.Store
import Data.Monoid

data Dir = L | R | U | D
  deriving (Eq)

instance Show Dir where
  show L = "<"
  show R = ">"
  show U = "^"
  show D = "v"

flipDir :: Dir -> Dir
flipDir U = D
flipDir L = R
flipDir D = U
flipDir R = L

nearby :: Bounds w h => Dir -> ISpace w h a -> a
nearby (indOf -> offsets) = peeks (<> offsets)

move :: Bounds w h => Dir -> ISpace w h a -> ISpace w h a
move (indOf -> offset) = seeks (<> offset)

data Neighbours a = Neighbours
  { left :: a
  , right :: a
  , up :: a
  , down :: a
  } deriving (Show, Eq, Functor)

instance Foldable Neighbours where
  foldMap f Neighbours{..} = f left <> f right <> f up <> f down

instance Distributive Neighbours where
  distribute = distributeRep

instance Representable Neighbours where
  type Rep Neighbours = Dir
  index n L = left n
  index n R = right n
  index n U = up n
  index n D = down n
  tabulate f =
    Neighbours { left=f L
              , right=f R
              , up=f U
              , down=f D
              }

neighbourPos :: Bounds w h => Neighbours (Ind w h)
neighbourPos = tabulate indOf

neighboursOf :: Bounds w h => ISpace w h a -> ISpace w h (Neighbours a)
neighboursOf = extend (experiment (\i -> fmap (<> i) neighbourPos))

indOf :: Bounds w h => Dir -> Ind w h
indOf U = (-1, 0)
indOf D = (1, 0)
indOf L = (0, -1)
indOf R = (0, 1)
