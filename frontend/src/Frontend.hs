{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Frontend where

import           Control.Lens                (( # ))

import qualified Data.Text                   as T

import           Reflex.Dom.Core

import           Common.Api
import           Static

import qualified Reflex.Dom.CanvasDyn        as Canvas

import           Reflex.Dom.Widget.SVG       (BasicSVG (Rectangle))
import qualified Reflex.Dom.Widget.SVG       as SVG

import           Reflex.Dom.Widget.SVG.Types (Height (..), SVG_El (..),
                                              SVG_Rect (..), Width (..), _PosX,
                                              _PosY)
import qualified Reflex.Dom.Widget.SVG.Types as SVGT

svgR :: SVG_Rect
svgR = SVG_Rect
  (_PosX # 10.0) (_PosY # 10.0)
  (Width 100.0) (Height 100.0)
  Nothing Nothing

svgEl :: SVG_El
svgEl = SVG_El (Width 200) (Height 200) Nothing

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = el "title" $ text "Obelisk Minimal Example"
    body = do
      text "Welcome to Obelisk!"

      _ <- SVG.svg_ (pure svgEl) $
        SVG.svgBasicDyn_ Rectangle SVGT.makeRectProps (pure svgR)

      el "p" $ text $ T.pack commonStuff
      elAttr "img" ("src" =: static @"obelisk.jpg") blank
