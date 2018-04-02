module Wirehack.Loop where

data Timer = Timer
data KeyPress = KeyPress Char
data TreasureCollected = TreasureCollected
data GetStatusInfo = GetStatusInfo

data GameState = GameState
makeLenses ''GameState

instance Default GameState where
  def = GameState

handleKeypress :: KeyPress -> App ()
handleKeypress (KeyPress c) = do
  case c of
    'a' -> pos -= 1
    'd' -> pos += 1
    _ -> return ()

keypressProvider :: EventDispatcher -> IO ()
keypressProvider dispatcher = forever $ do
  c <- getChar
  dispatcher (KeyPress c)

render :: App ()
render = return ()

timer :: EventDispatcher -> IO ()
timer dispatch = forever $ do
  threadDelay 1000000
  dispatch Timer

setup :: App ()
setup = do
  asyncEventProvider keypressProvider
  asyncEventProvider timer
  addListener_ handleKeypress
  afterEvent_ render
