{-# LANGUAGE TemplateHaskell #-}

module Wirehack.Cell where

import Wirehack.Neighbours

import Control.Lens hiding (Empty, index)
import Data.Default
import Data.Functor.Rep

-- A component is parameterized over the contents of each wire
-- Typically a direction is held within
data Component
  = Empty
  | Source
  | Sink
  | Cross
  | And
  | Wire Dir
  deriving (Eq)

instance Show Component where
  show Empty = "."
  show Source = "*"
  show Sink = "o"
  show Cross = "+"
  show And = "&"
  show (Wire d) = show d

data Cell = Cell
  { _component :: Component
  , _poweredBy :: Neighbours Bool
  }

makeLenses ''Cell

instance Show Cell where
  show cell = cell ^. component . to show

instance Default Cell where
  def = Cell {_component = Empty, _poweredBy = tabulate (const False)}

emp, source, cross, sink, and' :: Cell
emp = def

source = def & component .~ Source

sink = def & component .~ Sink

cross = def & component .~ Cross

and' = def & component .~ And

wire :: Dir -> Cell
wire d = def & component .~ Wire d
