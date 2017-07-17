{-# language DeriveFunctor #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Space where

import Index

import Data.Vector as V ((!), generate, Vector, fromList, zipWith)
import Data.List (intercalate)
import Data.Foldable (toList)
import Data.Monoid
import Data.Distributive
import Data.Functor.Rep
import Data.Typeable

import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Store

data ISpace x y a = ISpace (x, y) (Space x y a)
  deriving (Eq, Functor)

newtype Space x y a = Space (Vector (Vector a))
  deriving (Eq, Functor)

newtype Disp x y a = Disp (Space x y a)

instance Show (Disp x y Char) where
  show (Disp (Space v)) = intercalate "\n" . toList . fmap toList $ v

instance (Show a, Typeable x, Typeable y) => Show (Space x y a) where
  show (Space v) = foldMap ((<> "\n"). show) v

instance (Index x, Index y) => Distributive (Space x y) where
  distribute = distributeRep

instance (Index x, Index y) => Representable (Space x y) where
  type Rep (Space x y) = (x, y)
  index (Space v) (unwrapI -> x, unwrapI -> y) = v ! y ! x
  tabulate desc = Space $ generate numRows generator
    where
      numRows = unwrapI (maxBound :: x) + 1
      numCols = unwrapI (maxBound :: y) + 1
      generator x = generate numCols (\y -> desc (wrapI x, wrapI y))

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
      desc focus = ISpace focus v

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

g  :: Space Row Col (Row, Col)
g = tabulate id

basic :: Disp Row Col Char
basic = Disp $ fromLists
  [ "abc"
  , "def"
  , "xyz"
  ]
