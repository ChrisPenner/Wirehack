{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Wirehack.Marshal
  ( save
  , fromText
  ) where

import Control.Lens (use)
import Control.Monad.Trans (liftIO)
import Data.Foldable
import Data.Functor.Rep (Representable(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Eve (App)
import Safe (atMay)
import Wirehack.Cell (Cell(..), emp, sink, source, wire)
import Wirehack.Neighbours (Dir(..))
import Wirehack.Space (Bounds, ISpace(..), Ind, Mod(..), Space(..))
import Wirehack.State (space)

formatTime' :: FormatTime t => t -> String
formatTime' = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))

toText ::
     forall w h. (Bounds w h)
  => ISpace w h Cell
  -> T.Text
toText (ISpace _ (Space vect)) = T.unlines . toList . fmap toLine $ vect
  where
    toLine = foldMap (T.pack . show)

save :: App ()
save = do
  spc <- use space
  timeStr <- liftIO (formatTime' <$> getZonedTime)
  let fp = "wirehack-" ++ timeStr ++ ".hak"
  liftIO . TIO.writeFile fp . toText $ spc

fromText ::
     forall w h. (Bounds w h)
  => T.Text
  -> Maybe (Space w h Cell)
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
    buildSpace :: Ind w h -> Maybe Char
    buildSpace (Mod x, Mod y) = do
      line <- lines' `atMay` y
      if T.length line <= x
        then Nothing
        else Just (line `T.index` x)
    lines' = T.lines txt
