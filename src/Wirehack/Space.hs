{-# language DeriveFunctor #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language GADTs #-}
{-# language ConstraintKinds #-}
{-# language UndecidableInstances #-}
{-# language RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wirehack.Space where

import Data.Distributive
import Data.Functor.Rep
import Data.Functor.Compose
import Data.Monoid
import qualified Data.Stream.Infinite as S

import Control.Arrow
import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Store

import Control.Lens hiding (Index, index)

type D2 = Compose Space Space

overS :: (a -> a) -> Int -> S.Stream a -> S.Stream a
overS f 0 (x S.:> xs) = f x S.:> xs
overS f n (x S.:> xs)
  | n > 0 = x S.:> overS f (n - 1) xs
  | otherwise = error "Negative index in overS"

class (Representable r) => RepLens r where
  coord :: Rep r -> Lens' (r a) a

instance RepLens Space where
  coord ind = lens getter setter
    where
      getter = flip index ind
      setter (Space l r) new =
        if ind >= 0
            then Space l (overS (const new) ind r)
            else Space (overS (const new) (abs ind) l) r

instance (RepLens r, RepLens s)  => RepLens (Compose r s) where
  coord (indR, indS) = lens getter setter
    where
      getter (Compose rsa) = rsa ^. coord indR . coord indS
      setter (Compose rsa) new = Compose (rsa & coord indR %~ (coord indS .~ new))

focus :: (RepLens r) => Lens' (ISpace r a) a
focus = lens getter setter
  where
    getter = extract
    setter (ISpace foc s) new = ISpace foc (s & coord foc .~ new)

data Space a = Space
        (S.Stream a)
        (S.Stream a)
        deriving Functor

instance Distributive Space where
  distribute = distributeRep

instance Representable Space where
  type Rep Space = Int
  index (Space l r) i
    | i >= 0 = r S.!! i
    | otherwise = l S.!! (abs i - 1)

  tabulate desc = Space (S.unfold (desc &&& subtract 1) (-1)) (S.unfold (desc &&& (+1)) 0)

data ISpace r a where
  ISpace :: Representable r => Rep r -> r a -> ISpace r a

getSpace :: (Representable r) => ISpace r a -> r a
getSpace (ISpace _ r) = r


instance Functor (ISpace r) where
  fmap f (ISpace foc r) = ISpace foc (fmap f r)

instance Comonad (ISpace r) where
  extract (ISpace ind s) = index s ind
  duplicate (ISpace foc v) = ISpace foc $ tabulate desc
    where
      desc fc = ISpace fc v

instance (s ~ Rep r) => ComonadStore s (ISpace r) where
  pos (ISpace foc _) = foc
  peek foc (ISpace _ r) = index r foc

instance (e ~ Rep r) => ComonadEnv e (ISpace r) where
  ask = pos

instance (Monoid a) => Monoid (S.Stream a) where
  mempty = tabulate (const mempty)
  mappend = S.zipWith mappend

instance (Monoid a) => Monoid (Space a) where
  mempty = tabulate (const mempty)
  Space l r `mappend` Space l' r' = Space (l <> l') (r <> r')

idISpace :: ISpace D2 (Int, Int)
idISpace = ISpace (0, 0) idD2

idD2 :: D2 (Int, Int)
idD2 = tabulate id

idSpace :: Space Int
idSpace = tabulate id
