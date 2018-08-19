{-# LANGUAGE RankNTypes #-}

module Main where

import Wirehack.Display (renderSpace, stepGame)
import Wirehack.Events (handleKeypress)

import Control.Concurrent (threadDelay)
import Control.Monad
import Eve
  ( EventDispatcher
  , addListener_
  , afterEvent_
  , asyncEventProvider
  , eve_
  )
import Eve.CLI (initCLI, onKeypress_)

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
