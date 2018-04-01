{-# language TemplateHaskell #-}
module Wirehack.Components where

import Wirehack.Neighbours
import Data.Functor.Rep
import Control.Lens hiding (Empty, index)
import Data.Default
import qualified Graphics.Vty as V
import Data.Text.Lens

data Component = Empty | Source | PRight | PLeft | PUp | PDown
  deriving (Eq)

instance Show Component where
  show Empty = "."
  show Source = "o"
  show PRight = ">"
  show PLeft = "<"
  show PUp = "^"
  show PDown = "v"

data Cell = Cell 
  { _component :: Component
  , _src :: [Dir]
  , _powered :: Bool
  }

makeLenses ''Cell

instance Show Cell where
  show cell = cell ^. component . to show

instance Default Cell where
  def = Cell 
    { _component=Empty
    , _src=[]
    , _powered=False
    }

emp, source, pr, pl, pu, pd :: Cell
emp = def
source = def & component .~ Source & powered .~ True
pr = def & component .~ PRight
pl = def & component .~ PLeft
pu = def & component .~ PUp
pd = def & component .~ PDown

getPowerDir :: Component -> Neighbours Bool
getPowerDir Empty = tabulate (const False)
getPowerDir Source = tabulate (const True)
getPowerDir PUp = tabulate (== U)
getPowerDir PDown = tabulate (== D)
getPowerDir PLeft = tabulate (== L)
getPowerDir PRight = tabulate (== R)

powers :: Dir -> Cell -> Bool
powers d (Cell{_powered=True, _component=c}) = index (getPowerDir c) d
powers _ _ = False
