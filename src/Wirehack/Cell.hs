{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Wirehack.Cell
  ( component
  , poweredBy
  , emp
  , source
  , sink
  , cross
  , and'
  , wire
  , Cell(..)
  , Component(..)
  ) where

import Wirehack.Neighbours (Dir(..), Neighbours(..))

import Control.Lens (makeLenses)
import Data.Default (Default(..))
import Data.Functor.Rep (Representable(..))

-- | Each cell contains a component
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
  show Cell {_component} = show _component

-- The default cell is empty and unpowered
instance Default Cell where
  def = Cell {_component = Empty, _poweredBy = tabulate (const False)}

emp, source, cross, sink, and' :: Cell
emp = def

source = def {_component = Source}

sink = def {_component = Sink}

cross = def {_component = Cross}

and' = def {_component = And}

wire :: Dir -> Cell
wire d = def {_component = Wire d}
