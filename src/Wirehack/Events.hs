{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Wirehack.Events
  ( handleKeypress
  ) where

import Wirehack.Cell (and', cross, emp, sink, source, wire)
import Wirehack.Marshal (save)
import Wirehack.Neighbours (Dir(..), move)
import Wirehack.Space (focus)
import Wirehack.State (latch, latchMove, space)

import Eve
import Eve.CLI

import Control.Lens hiding (Empty, Index, index)
import Control.Monad.State
import Data.Char
import qualified Graphics.Vty as V

interrupt :: Keypress
interrupt = Keypress (V.KChar 'c') [V.MCtrl]

keyDir :: Char -> Maybe Dir
keyDir c =
  case toLower c of
    'h' -> Just L
    'l' -> Just R
    'j' -> Just D
    'k' -> Just U
    _ -> Nothing

keyChar :: Keypress -> Maybe Char
keyChar (Keypress (V.KChar c) _) = Just c
keyChar _ = Nothing

simpleMove :: Keypress -> App ()
simpleMove (Keypress (V.KChar 'H') _) = space . focus .= wire L
simpleMove (Keypress (V.KChar 'J') _) = space . focus .= wire D
simpleMove (Keypress (V.KChar 'K') _) = space . focus .= wire U
simpleMove (Keypress (V.KChar 'L') _) = space . focus .= wire R
simpleMove (Keypress (V.KChar 'l') _) = space %= move R
simpleMove (Keypress (V.KChar 'h') _) = space %= move L
simpleMove (Keypress (V.KChar 'k') _) = space %= move U
simpleMove (Keypress (V.KChar 'j') _) = space %= move D
simpleMove _ = return ()

handleKeypress :: Keypress -> App ()
handleKeypress k
  | k == interrupt = exit
handleKeypress (Keypress (V.KChar ' ') _) = latch %= not
handleKeypress (Keypress (V.KChar '*') _) = space . focus .= source
handleKeypress (Keypress (V.KChar 'o') _) = space . focus .= sink
handleKeypress (Keypress (V.KChar '+') _) = space . focus .= cross
handleKeypress (Keypress (V.KChar '.') _) = space . focus .= emp
handleKeypress (Keypress (V.KChar '&') _) = space . focus .= and'
handleKeypress (Keypress (V.KChar 's') [V.MCtrl]) = save
handleKeypress k = do
  b <- use latch
  case (guard b >> keyChar k >>= keyDir) of
    Just d -> latchMove d
    Nothing -> simpleMove k
