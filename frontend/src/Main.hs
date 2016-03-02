{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.Trans
import Data.Monoid
import Reflex.Dom

import qualified Data.Map as M

import Play

-- <div class="map" style="transform: scale(0.5); transform-origin: 0px 0px 0px;">
--   <div class="country rectangle" style="left:  1605px; top: 586px; width: 96px; height: 64px"
--     <div class="ts_sprite small_inf sprite_(su|us)_inf_(c|u)" style="left: 425px; top: 1216px; width: 96px; height: 64px;"> -- abstract width / height out
--     </div>
--   </div>
-- </div>
-- divs with glyph icons, letters added with content: "m" attribute
--
-- drag and dro is done somehow with the cursor: grab, cursor: grabbing, cursor: move; and grabbable
-- http://stackoverflow.com/questions/5697067/css-for-grabbing-cursors-drag-drop
--
-- map container has the overflow: scroll set and position absolute.  alxo box-sizing
--
-- .ts_sprite sets background-image to url("images/ts_sprites.jpg"), position: absolute; transform-origin: 0 0 0 not needed?
--
-- Card locations / names are needed by frontend, not by backend.


data Zoom = ZoomIn | ZoomOut
newtype ZoomLevel = ZoomLevel Int deriving Show


zoomDyn :: MonadWidget t m => Event t Zoom -> m (Dynamic t ZoomLevel)
zoomDyn zs = foldDyn newzoomfactor (ZoomLevel 10) zs
  where
    newzoomfactor ZoomIn (ZoomLevel i) =  ZoomLevel $ i+(1)
    newzoomfactor ZoomOut (ZoomLevel i) = ZoomLevel $ i-1


zoominbutton, zoomoutbutton :: MonadWidget t m => m (Event t Zoom)
zoomoutbutton = (ZoomOut <$) <$> button "zoomout"
zoominbutton = (ZoomIn <$) <$> button "zoomin"

mapAttrs :: forall t m. MonadWidget t m => Dynamic t ZoomLevel -> m (Dynamic t (M.Map String String))
mapAttrs factorEvent = do
  let
    transformAttr :: Int -> String
    transformAttr amt = "transform: scale(" <> show (fromIntegral amt / 10 :: Float) <> "); transform-origin: 0px 0px 0px;"

    attrs :: ZoomLevel -> M.Map String String
    attrs (ZoomLevel factor) = (M.fromList [("style", transformAttr factor )])

    scaleEvent :: Event t (M.Map String String)
    scaleEvent = fmap attrs (updated factorEvent) :: Event t (M.Map String String)

  holdDyn (attrs (ZoomLevel 10)) scaleEvent

main :: IO ()
main = mainWidget $ do
  rec
      (zoomin,zoomout) <- (,) <$> zoominbutton <*> zoomoutbutton
      zoom <- zoomDyn $ leftmost [zoomin, zoomout]
      attrsDyn <- mapAttrs zoom
      map <- elDynAttr "div" attrsDyn $ do
        display zoom
  return ()
