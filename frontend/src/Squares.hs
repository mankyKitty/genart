module Squares where

-- Have W/H
--
-- Generate squares of height N, with gutter of M, between W/H
-- remove duplicates

import Linear.V2 (V2)

import Reflex.Dom.Widget.SVG.Types (SVG_Rect)

data World = World
  { _worldHeight :: Height
  , _worldWidth  :: Width
  }
