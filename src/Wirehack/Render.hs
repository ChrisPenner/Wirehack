{-# language ViewPatterns #-}
{-# language FlexibleInstances #-}
module Wirehack.Render where

import Graphics.Vty as V
import Wirehack.Cell
import Wirehack.Space
import Wirehack.Power
import qualified Data.Text.Lazy as T
import Data.Vector (Vector)
import Control.Lens hiding (Empty)
import Control.Applicative

class Attrs a where
  attrs :: a -> V.Attr

instance Attrs Cell where
  attrs Cell{_component=Empty} = V.defAttr
  attrs cell = V.withForeColor V.defAttr $
    if hasPower cell
      then V.green
      else V.red

class Renderable a where
  render :: a -> V.Image

instance Renderable (ISpace w h Cell) where
  render spcd@(ISpace _ (Space spc)) =
    foldr (V.vertJoin . foldInner) V.emptyImage $ images
    where
      foldInner :: Vector V.Image -> V.Image
      foldInner = foldr V.horizJoin V.emptyImage
      cellAttrs = (focus <>~ highlighting) . fmap attrs $ spcd
      highlighting  = V.withStyle V.currentAttr V.reverseVideo
      displayedComponents = T.pack . show <$> spcd
      ISpace _ (Space images) = liftA2 V.text cellAttrs displayedComponents
