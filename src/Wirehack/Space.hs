{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
{-# language GADTs #-}
{-# language ConstraintKinds #-}
module Wirehack.Space where

import Data.Distributive
import Data.Functor.Rep
import Control.Comonad
import Control.Comonad.Env
import Data.Functor.Compose
import Data.Monoid
import Data.Vector as V
import Control.Lens hiding (index)
import qualified Data.Stream.Infinite as S
import GHC.TypeLits
import Data.Proxy
import Control.Applicative

import Control.Comonad.Representable.Store

type Bounds w h = (KnownNat w, KnownNat h)

data ISpace width height a where
  ISpace :: Bounds w h => (Sum Int, Sum Int) -> (Space w h a) -> ISpace w h a

instance Functor (ISpace w h) where
  fmap f (ISpace ind spc) = ISpace ind (fmap f spc)

instance Bounds w h => Applicative (ISpace w h) where
  pure = ISpace (0, 0) . pure
  (ISpace indA a) <*> (ISpace _ b) = ISpace indA (a <*> b)

data Space (width :: Nat) (height :: Nat) a where
  Space :: Bounds w h => (Vector (Vector a)) -> Space w h a

instance Functor (Space w h) where
  fmap f (Space spc) = Space (fmap (fmap f) spc)

instance Bounds w h => Applicative (Space w h) where
  pure = tabulate . const
  (Space a) <*> (Space b) = Space $ V.zipWith (V.zipWith ($)) a b

instance Bounds w h => Comonad (ISpace w h) where
  extract (ISpace ind spc) = index spc ind
  extend f (ISpace ind spc) =
    ISpace ind (tabulate (\ix -> f (ISpace ix spc)))

instance Bounds w h => ComonadStore (Sum Int, Sum Int) (ISpace w h) where
  pos (ISpace ind _) = ind
  peek ind (ISpace _ spc) = index spc ind

instance Bounds w h => Distributive (Space w h) where
  distribute = distributeRep

instance Bounds w h => Representable (Space w h) where
  type Rep (Space x y) = (Sum Int, Sum Int)
  index (Space spc) (Sum x, Sum y) = spc ! x ! y
  tabulate f = Space $ V.generate width (\ x -> V.generate height (\y -> f (Sum x, Sum y)))
    where
      width = fromIntegral $ natVal (Proxy :: Proxy w)
      height = fromIntegral $ natVal (Proxy :: Proxy h)

focus :: Bounds w h => Lens' (ISpace w h a) a
focus = lens getter setter
  where
    getter spc@(ISpace _ _) = extract spc
    setter (ISpace ind@(Sum x, Sum y) (Space spc)) new = ISpace ind . Space $ spc // [(x, newInner)]
      where
        nestedVal = spc ! x
        newInner = nestedVal // [(y, new)]
