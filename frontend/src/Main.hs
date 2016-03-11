{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Control.Monad.Trans
import Control.Monad (when,forM)
import Data.Monoid
import Reflex.Dom

import qualified Data.Map as M

import Countries

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


data Zoom = ZoomIn | ZoomOut | ZoomDefault
newtype ZoomLevel = ZoomLevel Float deriving Show


newZoomDyn :: MonadWidget t m => ZoomLevel -> Event t Zoom -> m (Dynamic t ZoomLevel)
newZoomDyn init zs =
  let
    newzoomfactor ZoomIn (ZoomLevel i) =  ZoomLevel $ min 15 $ i * 1.1
    newzoomfactor ZoomOut (ZoomLevel i) = ZoomLevel $ max 1 $ i * 0.9
    newzoomfactor ZoomDefault _ = init

  in foldDyn newzoomfactor init zs

zoominbutton, zoomoutbutton, zoomdefbutton :: MonadWidget t m => m (Event t Zoom)
zoomoutbutton = (ZoomOut <$) <$> button "zoom out"
zoominbutton = (ZoomIn <$) <$> button "zoom in"
zoomdefbutton = (ZoomDefault <$) <$> button "default zoom"

mapWidget :: MonadWidget t m => M.Map Country Influence -> Event t Zoom -> m ()
mapWidget inf zoomEvents = do
  let
    transformAttr :: ZoomLevel -> String
    transformAttr (ZoomLevel factor) = "transform: scale(" <> show (factor / 10) <> "); transform-origin: 0px 0px 0px;"

    attrs :: ZoomLevel -> M.Map String String
    attrs zoomlevel = [
      ("id", "map"),
      ("style", "position: relative; left: 0; top: 0; " <> transformAttr zoomlevel)
      ]

    defaultZoom = ZoomLevel 4

  zoomDyn <- newZoomDyn defaultZoom zoomEvents
  attrsDyn <- holdDyn (attrs defaultZoom) (attrs <$> updated zoomDyn)

--   mapStr <- mapDyn (\zoom -> "This is the map.  " <> show zoom) zoomDyn

  elDynAttr "div" attrsDyn $ do
    elAttr "img" [("src", "file:///home/toad/bit/twilightstrugglemap.jpg")] (text "")
    forM influenceTest $ \(country, (usinf,ussrinf) ) -> do
      let (Offset left top) = M.findWithDefault (Offset 0 0) country countryLocs

      -- TODO this stuff needs to go into css filej
      when (usinf > 0) $ do
        elAttr "div" [("style","width: 65px; height: 64px; top: " <> show top <> "px; left: " <> show left <> "px; position: absolute; font-size: 48px; font-weight: bold; color: white; background-color: blue; text-align: center; margin-top: auto; margin-bottom: auto;")] (text (show usinf))
      when (ussrinf > 0) $
        elAttr "div" [("style","width: 66px; height: 64px; top: " <> show top <> "px; left: " <> show (left + 65) <> "px; position: absolute; font-size: 48px; font-weight: bold; color: white; background-color: red; text-align: center; margin-top: auto; margin-bottom: auto;")] (text (show ussrinf))
  return ()

-- countryDyn :: CountryOp -> m ()

main :: IO ()
main = mainWidget $ do
  rec
    zoomEvents<- (\i o d -> leftmost [i,o,d]) <$> zoominbutton <*> zoomoutbutton <*> zoomdefbutton
    mapWidget [] $ leftmost [zoomEvents]
  return ()
