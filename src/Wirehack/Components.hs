module Wirehack.Components where

data Component = Empty | Source | PRight | PLeft | PUp | PDown
  deriving (Eq)

instance Show Component where
  show Empty = "."
  show Source = "o"
  show PRight = ">"
  show PLeft = "<"
  show PUp = "^"
  show PDown = "v"
