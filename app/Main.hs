{-# language RankNTypes #-}
module Main where

import Wirehack.Display

import Eve
import Eve.CLI
import Control.Concurrent
import Control.Monad

data Timer = Timer

main :: IO ()
main = eve_ $ do
  initCLI
  asyncEventProvider timer
  addListener_ (\Timer -> stepGame)
  onKeypress_ handleKeypress
  afterEvent_ renderSpace
  renderSpace

timer :: EventDispatcher -> IO ()
timer dispatch = forever $ do
  dispatch Timer
  threadDelay 500000
