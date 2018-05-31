{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend where

import           Control.Monad.IO.Class      (liftIO)

import           Control.Monad.Random        (RandT, RandomGen)
import qualified Control.Monad.Random        as Rnd

import           Control.Monad.Reader        (ReaderT, runReaderT)

import           System.Random               (StdGen)

import           Control.Lens                (from, to, under, ( # ), (%~),
                                              (^.), _Wrapped)
import           Control.Monad               (void)

import           Data.Semigroup              ((<>))

import           Data.Foldable               (traverse_)
import           Data.Functor.Identity       (Identity)
import           Data.List                   (intercalate)

import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import           Data.Map                    (Map)
import qualified Data.Map                    as Map

import           Data.Colour                 (Colour)
import qualified Data.Colour.Names           as Col
import qualified Data.Colour.SRGB            as Col

import qualified Numeric.Noise.Perlin        as Perlin

import qualified Reflex                      as R
import           Reflex.Dom.Core

import qualified Reflex.Dom.CanvasDyn        as Canvas

import           Reflex.Dom.Widget.SVG       (BasicSVG (Path))
import qualified Reflex.Dom.Widget.SVG       as SVG

import           Reflex.Dom.Widget.SVG.Types (Height (..), Pos, SVG_El (..), SVG_Polygon,
                                              Width (..), X, Y, _PosX, _PosY)
import qualified Reflex.Dom.Widget.SVG.Types as SVGT

import           Squares

textColour
  :: ( RealFrac a
     , Floating a
     )
  => Colour a
  -> Text
textColour =
  Text.pack . Col.sRGB24show

bgProps :: Map Text Text
bgProps = "style" =:
  ( mconcat
  [ "background-color:" <> textColour Col.ivory <> ";"
  , "transform-origin:top left;"
  , "transform:scale(10);"
  ]
  )

wheight :: Height
wheight = Height 60

wwidth :: Width
wwidth = Width 60

svgEl :: SVG_El
svgEl = SVG_El wwidth wheight Nothing

mkPathAttrs p = Map.fromList
  [ ("stroke", textColour Col.black)
  , ("stroke-width", "0.2px")
  , ("fill", "none")
  ] <> SVGT.makePolygonProps p

renderPoly
  :: MonadWidget t m
  => Dynamic t (SVG_Polygon -> SVG_Polygon)
  -> Dynamic t SVG_Polygon
  -> m ()
renderPoly dPerlin dPoly = void
  $ SVG.svgBasicDyn_ SVG.Polygon mkPathAttrs (dPerlin <*> dPoly)

runGen
  :: RandomGen g
  => g
  -> r
  -> ReaderT r (RandT g Identity) a
  -> (a, g)
runGen sGen r a =
  Rnd.runRand (runReaderT a r) sGen

generatePolys
  :: RandomGen g
  => World
  -> Size
  -> g
  -> ([SVGT.SVG_Polygon], g)
generatePolys wrld sz gen = runGen gen wrld $
  createManyPolys sz (Padding 3) (Count 800)

addPerlinNoise
  :: RandomGen g
  => g
  -> SVGT.SVG_Polygon
  -> SVGT.SVG_Polygon
addPerlinNoise gen poly =
  let
    seed = fst $ Rnd.randomR (2.0,5.0) gen
    octaves = 5
    scale = 0.06
    persistance = 0.5

    noise = Perlin.perlin (round seed) octaves scale persistance

    perlin2d x y = Perlin.noiseValue noise (x + seed, y + seed, seed) - 0.5

    addNoise (x,y) =
      let
        noise' = realToFrac $ perlin2d
          (x ^. _Wrapped . to realToFrac)
          (y ^. _Wrapped . to realToFrac)

        addN :: Pos s -> Pos s
        addN = under (from _Wrapped) (+ noise')
      in
        (addN x, addN y)
  in
    poly
    & SVGT.svg_polygon_start %~ addNoise
    & SVGT.svg_polygon_path . traverse %~ addNoise

size :: Size
size = Size 1.5

wrld :: World
wrld = World wheight wwidth 2

body :: StdGen -> Widget x ()
body sGen = do
  eGenerate <- button "Generate"

  let
    dSvgRootAttrs = pure $
      bgProps <> SVGT.makeSVGProps svgEl

  rec (dPolys, dGen) <- splitDynPure <$> holdDyn (mempty, sGen)
        (generatePolys wrld size <$> current dGen <@ eGenerate)

  dPerlinNoise <- holdDyn (addPerlinNoise sGen)
    $ addPerlinNoise <$> current dGen <@ eGenerate

  void . SVG.svgElDynAttr' SVG.SVG_Root dSvgRootAttrs $
    simpleList dPolys (renderPoly dPerlinNoise)

frontend :: StdGen -> (StaticWidget x (), Widget x ())
frontend sGen =
  ( el "title" $ text "Oh my"
  , body sGen
  )
