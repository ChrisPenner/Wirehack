{-# language TemplateHaskell #-}
module Wirehack.Cell where

import Wirehack.Neighbours

import Data.Functor.Rep
import Control.Lens hiding (Empty, index)
import Data.Default
import qualified Graphics.Vty as V
import Data.Text.Lens

data Component = Empty | Source | Sink | Cross | PRight | PLeft | PUp | PDown
  deriving (Eq)

instance Show Component where
  show Empty = "."
  show Source = "*"
  show Sink = "o"
  show Cross = "+"
  show PRight = ">"
  show PLeft = "<"
  show PUp = "^"
  show PDown = "v"

data Cell = Cell
  { _component :: Component
  , _poweredBy :: Neighbours Bool
  }

makeLenses ''Cell

instance Show Cell where
  show cell = cell ^. component . to show

instance Default Cell where
  def = Cell
    { _component=Empty
    , _poweredBy=tabulate (const False)
    }

emp, source, cross, pr, pl, pu, pd :: Cell
emp = def
source = def & component .~ Source
sink = def & component .~ Sink
cross = def & component .~ Cross
pr = def & component .~ PRight
pl = def & component .~ PLeft
pu = def & component .~ PUp
pd = def & component .~ PDown
