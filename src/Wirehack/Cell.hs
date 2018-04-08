{-# language TemplateHaskell #-}
module Wirehack.Cell where

import Wirehack.Neighbours

import Data.Functor.Rep
import Control.Lens hiding (Empty, index)
import Data.Default

data Component a =
      Empty
    | Source
    | Sink
    | Cross
    | And
    | Wire a
  deriving (Eq)

instance Show a => Show (Component a) where
  show Empty = "."
  show Source = "*"
  show Sink = "o"
  show Cross = "+"
  show And = "&"
  show (Wire d) = show d

data Cell = Cell
  { _component :: Component Dir
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

emp, source, cross, sink, and' :: Cell
emp = def
source = def & component .~ Source
sink = def & component .~ Sink
cross = def & component .~ Cross
and' = def & component .~ And

wire :: Dir -> Cell
wire d = def & component .~ Wire d
