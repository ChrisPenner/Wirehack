{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
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

import Control.Comonad.Representable.Store

data ISpace width height a = ISpace (Sum Int, Sum Int) (Space width height a)
  deriving Functor
data Space (width :: Nat) (height :: Nat) a = Space (Vector (Vector a))
  deriving Functor

instance (KnownNat width, KnownNat height) => Comonad (ISpace width height) where
  extract (ISpace ind spc) = index spc ind
  extend f (ISpace ind spc) =
    ISpace ind (tabulate (\ix -> f (ISpace ix spc)))

instance (KnownNat width, KnownNat height) => ComonadStore (Sum Int, Sum Int) (ISpace width height) where
  pos (ISpace ind _) = ind
  peek ind (ISpace _ spc) = index spc ind

instance (KnownNat width, KnownNat height) => Distributive (Space width height) where
  distribute = distributeRep

instance (KnownNat width, KnownNat height) => Representable (Space width height) where
  type Rep (Space x y) = (Sum Int, Sum Int)
  index (Space spc) (Sum x, Sum y) = spc ! x ! y
  tabulate f = Space $ V.generate width (\ x -> V.generate height (\y -> f (Sum x, Sum y)))
    where
      width = fromIntegral $ natVal (Proxy :: Proxy width)
      height = fromIntegral $ natVal (Proxy :: Proxy height)

focus :: (KnownNat w, KnownNat h) => Lens' (ISpace w h a) a
focus = lens getter setter
  where
    getter = extract
    setter (ISpace ind@(Sum x, Sum y) (Space spc)) new = ISpace ind . Space $ spc // [(x, newInner)]
      where
        nestedVal = spc ! x
        newInner = nestedVal // [(y, new)]
