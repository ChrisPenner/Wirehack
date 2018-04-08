{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
module Wirehack.Events where

import Wirehack.Cell
import Wirehack.Space
import Wirehack.Neighbours
import Wirehack.State
import Data.Char

import Eve
import Eve.CLI


import Control.Monad.State
import Control.Lens hiding (Index, Empty, index)

import qualified Graphics.Vty as V

type HackM w h a = StateT (ISpace w h Cell) IO a

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
handleKeypress k | k == interrupt = exit
handleKeypress (Keypress (V.KChar ' ') _) = latch %= not
handleKeypress (Keypress (V.KChar '*') _) = space . focus .= source
handleKeypress (Keypress (V.KChar 'o') _) = space . focus .= sink
handleKeypress (Keypress (V.KChar '+') _) = space . focus .= cross
handleKeypress (Keypress (V.KChar '.') _) = space . focus .= emp
handleKeypress (Keypress (V.KChar '&') _) = space . focus .= and'
handleKeypress k = do
  b <- use latch
  case (guard b >> keyChar k >>= keyDir) of
    Just d -> latchMove d
    Nothing -> simpleMove k
