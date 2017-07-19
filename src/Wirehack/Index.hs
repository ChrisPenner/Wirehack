{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wirehack.Index (Row, Col, Index(..), row, col, addInd) where

newtype Row = Row Int
  deriving (Show, Eq, Ord)

newtype Col = Col Int
  deriving (Show, Eq, Ord)

class (Bounded x, Ord x) => Index x where
  wrapI :: Int -> x
  unwrapI :: x -> Int

instance Bounded Col where
  minBound = Col 0
  maxBound = Col 15

instance Bounded Row where
  minBound = Row 0
  maxBound = Row 10

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

addInd :: (Index x, Index y) => (x, y) -> (Int, Int) -> (x, y)
addInd (unwrapI -> x, unwrapI -> y) (x', y') = (wrapI (x + x'), wrapI (y + y'))
