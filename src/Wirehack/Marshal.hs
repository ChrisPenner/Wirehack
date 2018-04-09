{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
module Wirehack.Marshal where

import Eve
import Wirehack.Cell
import Wirehack.Space
import Wirehack.Neighbours
import Data.Functor.Rep
import Data.Time
import Wirehack.State
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Trans
import Safe
import Data.Foldable
import Control.Lens

formatTime' :: FormatTime t => t -> String
formatTime' = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))

toText :: forall w h. (Bounds w h) => ISpace w h Cell -> T.Text
toText (ISpace _ (Space vect)) =
  T.unlines . toList . fmap toLine $ vect
    where
      toLine = foldMap (T.pack . show)

save :: App ()
save = do
  spc <- use space
  timeStr <- liftIO (formatTime' <$> getZonedTime)
  let fp = "wirehack-" ++ timeStr ++ ".hak"
  liftIO . TIO.writeFile fp . toText $ spc

fromText :: forall w h. (Bounds w h) => T.Text -> Maybe (Space w h Cell)
fromText txt = sequenceA $ tabulate (fmap cellFromChar . buildSpace)
  where
    cellFromChar :: Char -> Cell
    cellFromChar '.' = emp
    cellFromChar '*' = source
    cellFromChar 'o' = sink
    cellFromChar '>' = wire R
    cellFromChar '<' = wire L
    cellFromChar '^' = wire U
    cellFromChar 'v' = wire D
    cellFromChar _ = emp
    buildSpace ::  Ind w h -> Maybe Char
    buildSpace (Mod x, Mod y) = do
      line <- lines' `atMay` y
      if T.length line <= x
         then Nothing
         else Just (line `T.index` x)
    lines' = T.lines txt

