{-# language DeriveFunctor #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Space where

import Index

import Data.Vector ((!), generate, Vector, fromList)
import Data.List (intercalate)
import Data.Foldable (toList)
import Data.Monoid
import Data.Distributive
import Data.Functor.Rep
import Data.Typeable

import Control.Comonad
import Control.Comonad.Env

data ISpace x y a = ISpace (Ind x, Ind y) (Space x y a)
  deriving (Eq, Functor)

data Space x y a = Space (Vector (Vector a))
  deriving (Eq, Functor)

newtype Disp x y a = Disp (Space x y a)

instance Show (Disp x y Char) where
  show (Disp (Space v)) = intercalate "\n" . toList . fmap toList $ v

instance (Show a, Typeable x, Typeable y) => Show (Space x y a) where
  show (Space v) = foldMap ((<> "\n"). show) v

instance (Index x, Index y) => Distributive (Space x y) where
  distribute = distributeRep

instance (Index x, Index y) => Representable (Space x y) where
  type Rep (Space x y) = (Ind x, Ind y)
  index (Space v) (unwrapI -> x, unwrapI -> y) = v ! y ! x
  tabulate desc = Space $ generate numRows generator
    where
      numRows = unwrapI (maxBound :: Ind x) + 1
      numCols = unwrapI (maxBound :: Ind y) + 1
      generator x = generate numCols (\y -> desc (wrapI x, wrapI y))

instance (Index x, Index y) => Comonad (ISpace x y) where
  extract (ISpace ind s) = index s ind
  duplicate (ISpace foc v) = ISpace foc $ tabulate desc
    where
      desc focus = ISpace focus v

instance (Index x, Index y) => ComonadEnv (Ind x, Ind y) (ISpace x y) where
  ask (ISpace foc _) = foc

moveTo :: (Index x, Index y) => (Ind x, Ind y) -> ISpace x y a -> ISpace x y a
moveTo foc (ISpace _ v) = ISpace foc v

moveBy :: (Index x, Index y) => (Ind x, Ind y) -> ISpace x y a -> ISpace x y a
moveBy offs (ISpace curr v)
  = ISpace (curr + offs) v

fromLists :: (Index x, Index y) => [[a]] -> Space x y a
fromLists xs = Space $ generate (length xs) rows
  where
    rows i = fromList (xs !! i)

g  :: Space Row Col (Ind Row, Ind Col)
g = tabulate id

basic :: Disp Row Col Char
basic = Disp $ fromLists
  [ "abc"
  , "def"
  , "xyz"
  ]
