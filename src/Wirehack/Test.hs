{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language StandaloneDeriving #-}
module Wirehack.Test where

import GHC.TypeLits
import Data.Functor.Compose

type family Count t where
  Count (Compose _ g) = 1 + (Count g)
  Count _ = 0

data Grid (n::Nat) a where
  Grid :: f a -> Grid (Count f) a

f :: a -> a
f a = id a


-- type family Count t where
--   Count (_, x) = (Count x) + 1
--   Count _ = 0

-- type family BaseType t where
--   BaseType (_, x) = BaseType x
--   BaseType a = a

-- data Dim (n :: Nat) where
--   Dim :: (Count t ~ n) => t -> Dim n

-- class Promote (m :: Nat) (n :: Nat) where
--   promote :: Dim m -> Dim n

-- instance (m ~ n) => Promote m n where
--   promote (Dim t) = Dim t

-- instance ((m + 1) ~ n) => Promote m n where
--   promote (Dim t) = Dim t
