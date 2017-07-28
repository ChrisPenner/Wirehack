module Wirehack.Dim where

import Wirehack.Space
import Data.Functor.Rep
import Data.Distributive
import Data.Functor.Compose

class (Representable f) => Rangeable f where
  type Range f a :: *
  getRange :: (Rep f, Rep f) -> f a -> Range f a

instance Rangeable Space where
  type Range Space a = [a]
  getRange (start, end) fa = index fa <$> [start .. end]

instance (Representable f , Enum (Rep f) , Rangeable g)
         => Rangeable (Compose f g) where
  type Range (Compose f g) a = [Range g a]
  getRange ((start, srest), (end, erest)) (Compose fga) =
    getRange (srest, erest) . index fga <$> [start..end]


flipXY :: (Functor f, Distributive g) => Compose f g a -> Compose g f a
flipXY = Compose . distribute . getCompose
