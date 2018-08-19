{-# LANGUAGE RankNTypes #-}

module Main where

import Wirehack.Display
import Wirehack.Events

import Control.Concurrent
import Control.Monad
import Eve
import Eve.CLI

data Timer =
  Timer

main :: IO ()
main =
  eve_ $ do
    initCLI
    asyncEventProvider timer
    addListener_ (\Timer -> stepGame)
    onKeypress_ handleKeypress
    afterEvent_ renderSpace
    renderSpace

timer :: EventDispatcher -> IO ()
timer dispatch =
  forever $ do
    dispatch Timer
    threadDelay 100000
