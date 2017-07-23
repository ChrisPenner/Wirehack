module Wirehack.Components where

data Component = Empty | Up | Down
  deriving (Eq)

instance Show Component where
  show Empty = "."
  show Up = "U"
  show Down = "D"

instance Monoid Component where
  mempty = Empty
  x `mappend` Empty = x
  _ `mappend` x = x

toggleC :: Component -> Component
toggleC Empty = Up
toggleC Up = Down
toggleC Down = Empty
