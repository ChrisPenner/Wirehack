module Main where

import Wirehack.Display

import Eve
import Eve.CLI

main :: IO ()
main = runCLI $ do
  asyncEventProvider timer
  onKeypress_ (\k -> handleKeypress k *> renderSpace)
  addListener_ (\Timer -> stepGame)
  renderSpace
