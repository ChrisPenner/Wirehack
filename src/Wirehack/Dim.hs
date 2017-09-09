{-# language TypeFamilies #-}
{-# language DataKinds #-}
{-# language MultiParamTypeClasses #-}
{-# language GADTs #-}
{-# language UndecidableInstances #-}
{-# language AllowAmbiguousTypes #-}
{-# language ScopedTypeVariables #-}
{-# language PolyKinds #-}
{-# language FlexibleInstances #-}
module Wirehack.Dim where

import Wirehack.Space
import Data.Functor.Rep
import Data.Distributive
import Data.Functor.Compose

data Nat = S Nat | Z

-- instance (Representable f) => Distributive (Comp f) where
--   distribute = distributeRep

-- instance Representable f => Representable (Comp f) where
--   Rep (Comp f) = (Rep f, Rep)

data Comp (n :: Nat) (f :: * -> *) a where
  CompC :: f (Comp n f a) -> Comp (S n) f a
  CompF :: f a -> Comp Z f a


instance (Functor f) => Functor (Comp n f) where
  fmap f (CompF fa) = CompF $ fmap f fa
  fmap f (CompC fa) = CompC $ fmap f <$> fa

instance Representable f => Distributive (Comp Z f) where
  distribute = distributeRep

instance Representable f => Distributive (Comp (S n) f) where
  distribute = undefined -- distributeRep

instance Representable f => Representable (Comp Z f) where
  type Rep (Comp Z f) = RepOf (Comp Z f)
  index (CompF fa) = index fa
  tabulate desc = CompF (tabulate desc)

-- instance (Representable f
--          , Representable (Comp n f)
--          , Rep (Comp (S n) f) ~ (Rep f, Rep (Comp n f))
--          ) => Representable (Comp (S n) f) where
--   type Rep (Comp (S n) f) = RepOf (Comp (S n) f)
--   index (CompC fs) (i, rest) = index (index fs i) rest
--   tabulate desc = CompC (tabulate (\i -> desc (i, tabulate _)))


type family CountComp f :: Nat where
  CountComp (g c) = S (CountComp c)
  CountComp c = Z

type family RepOf c where
  RepOf (Comp n f) = CompRep n f

type family CompRep (n :: Nat) r where
  CompRep Z x = Rep x
  CompRep (S n) x = (Rep x, CompRep n x)

class (Representable r, Representable s) => Promote r s where
  liftInd :: Rep r -> Rep s

instance (Representable r) => Promote r r where
  liftInd = id

-- instance ( Representable (Comp m f)
--          , Representable (Comp (S m) f)
--          , Rep f ~ Int
--          ) => Promote (Comp m f) (Comp (S m) f) where
--            liftInd ind = (0 :: Int, ind)

type family Range f :: * -> * where
  Range (Compose f g) = Compose [] (Range g)
  Range _ = []

data Ind  (r :: * -> *) where
  Ind :: (Representable r) => Rep r -> Ind r

class Rangeable f where
  getRange :: (Rep f, Rep f) -> f a -> (Range f) a

-- instance (Representable f, Enum (Rep f), Range f ~ []) => Rangeable f where
instance Rangeable Space where
  getRange (start, end) fa = index fa <$> [start .. end]

instance (Representable f, Enum (Rep f) , Rangeable g)
         => Rangeable (Compose f g) where
  getRange ((start, srest), (end, erest)) (Compose fga) =
    Compose (getRange (srest, erest) . index fga <$> [start..end])

flipXY :: (Functor f, Distributive g) => Compose f g a -> Compose g f a
flipXY = Compose . distribute . getCompose
