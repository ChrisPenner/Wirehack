module Wirehack.Components where

data Component = Empty | All
  deriving (Eq)

instance Show Component where
  show Empty = "."
  show All = "+"

toggleC :: Component -> Component
toggleC Empty = All
toggleC All = Empty
