module Wirehack.Components where

data Component = Empty | All
  deriving (Eq)

instance Show Component where
  show Empty = "."
  show All = "+"

instance Monoid Component where
  mempty = Empty
  x `mappend` Empty = x
  _ `mappend` x = x

toggleC :: Component -> Component
toggleC Empty = All
toggleC All = Empty
