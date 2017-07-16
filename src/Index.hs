{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language ExistentialQuantification #-}
{-# language UndecidableInstances #-}
module Index (Row, Col, Index(..), Ind) where

import Data.Typeable

data Row
data Col

newtype Ind t = Ind Int
  deriving (Eq, Ord)

instance (Typeable t) => Show (Ind t) where
  show (Ind x) = show (typeRep (Proxy :: Proxy t)) ++ " " ++ show x

instance Bounded (Ind Col) where
  minBound = Ind 0
  maxBound = Ind 3

instance Bounded (Ind Row) where
  minBound = Ind 0
  maxBound = Ind 3

instance Enum (Ind t) where
  toEnum = Ind
  fromEnum (Ind i) = i

clamp :: (Ord a, Bounded a) => a -> a
clamp = min maxBound . max minBound

instance (Bounded (Ind bnd)) => Num (Ind bnd) where
  Ind a + Ind b = clamp (Ind (a + b))
  negate (Ind a) = clamp $ Ind (-a)
  Ind a * Ind b = clamp (Ind (a * b))
  abs (Ind a) = clamp (Ind a)
  signum (Ind a) = Ind (signum a)
  fromInteger = clamp . Ind . fromIntegral

instance (Num (Ind x), Num (Ind y)) => Num (Ind x, Ind y) where
  (x, y) + (x', y') = (x + x', y + y')
  negate (x, y) = (negate x, negate y)
  (x, y) * (x', y') = (x * x', y * y')
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger = error "Can't Construct Index from single int; use (x, y) instead"

class (Bounded (Ind x)) => Index x where
  unwrapI :: Ind x -> Int
  wrapI :: Int -> Ind x

instance Index Col where
  unwrapI (Ind c) = c
  wrapI c = clamp (Ind c)

instance Index Row where
  unwrapI (Ind r) = r
  wrapI r = clamp (Ind r)

