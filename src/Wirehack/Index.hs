{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wirehack.Index (Row, Col, Index(..), row, col) where

newtype Row = Row Int
  deriving (Show, Eq, Ord)

newtype Col = Col Int
  deriving (Show, Eq, Ord)

class (Bounded x, Ord x, Num x) => Index x where
  wrapI :: Int -> x
  unwrapI :: x -> Int

instance Bounded Col where
  minBound = Col 0
  maxBound = Col 5

instance Bounded Row where
  minBound = Row 0
  maxBound = Row 3

instance (Index x) => Enum x where
  toEnum = wrapI
  fromEnum = unwrapI

instance Index Row where
  wrapI = clamp . Row
  unwrapI (Row r) = r

row :: Int -> Row
row = wrapI
col :: Int -> Col
col = wrapI

instance Index Col where
  wrapI = clamp . Col
  unwrapI (Col r) = r

clamp :: Index a => a -> a
clamp = min maxBound . max minBound

iPlus :: Index x => x -> x -> x
iPlus a b = wrapI (unwrapI a + unwrapI b)
iNegate :: Index x => x -> x
iNegate a = wrapI (- (unwrapI a))
iMult :: Index x => x -> x -> x
iMult a b = wrapI (unwrapI a * unwrapI b)
iAbs :: Index x => x -> x
iAbs = wrapI . abs . unwrapI
iSignum :: Index x => x -> x
iSignum = wrapI . signum . unwrapI
iFromInteger :: Index x => Integer -> x
iFromInteger = wrapI . fromInteger

instance Num Row where
  (+) = iPlus
  negate = iNegate
  (*) = iMult
  abs = iAbs
  signum = iSignum
  fromInteger = iFromInteger

instance Num Col where
  (+) = iPlus
  negate = iNegate
  (*) = iMult
  abs = iAbs
  signum = iSignum
  fromInteger = iFromInteger
