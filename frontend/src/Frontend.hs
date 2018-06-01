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

import           Control.Lens                (from, fusing, over, to, under, at,
                                              ( # ), (%~), (^.), (^?), _1, _2,
                                              _Wrapped)
import           Control.Monad               (void)

import Data.Maybe (fromMaybe)
import           Data.Semigroup              ((<>))

import           Data.Time                   (UTCTime, getCurrentTime)

import           Data.Foldable               (traverse_,fold)
import           Data.Functor.Identity       (Identity)
import           Data.List                   (intercalate)
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.List.NonEmpty          as NE

import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import           Data.Map                    (Map)
import qualified Data.Map                    as Map

import           Data.Colour                 (Colour)
import qualified Data.Colour.Names           as Col
import qualified Data.Colour.SRGB            as Col

import           Numeric.Noise               (Seed)
import qualified Numeric.Noise.Perlin        as Perlin

import qualified Reflex                      as R
import           Reflex.Dom.Core

import qualified Reflex.Dom.CanvasDyn        as Canvas

import           Reflex.Dom.Widget.SVG       (BasicSVG (Path), BasicInner (..))
import qualified Reflex.Dom.Widget.SVG       as SVG

import           Reflex.Dom.Widget.SVG.Types (Height (..), Pos, SVG_El (..),
                                              SVG_Path, Width (..), X, Y, _PosX,
                                              _PosY)
import qualified Reflex.Dom.Widget.SVG.Types as SVGT

import           Squares
import           Static                      (static)

bgProps :: Map Text Text
bgProps = "class" =: "svg-wrapper"

wheight :: Height
wheight = Height 60

wwidth :: Width
wwidth = Width 60

svgEl :: SVG_El
svgEl = SVG_El wwidth wheight Nothing

generatePolys
  :: RandomGen g
  => World
  -> Size
  -> Padding
  -> Count
  -> g
  -> (NonEmpty Poly, g)
generatePolys wrld sz padding count =
  Rnd.runRand (runReaderT (createPolygons sz padding count) wrld)

addNoise
  :: Seed
  -> (Pos X, Pos Y)
  -> (Pos X, Pos Y)
addNoise seed (x,y) =
  let
    seed' = fromIntegral seed
    octaves = 5
    scale = 0.06
    persistance = 0.5
    noise = Perlin.perlin seed octaves scale persistance

    x' = x ^. _Wrapped . to realToFrac
    y' = y ^. _Wrapped . to realToFrac

    noiseVal = realToFrac $
      Perlin.noiseValue noise (x' + seed', y' + seed', seed') - 0.5

    addN :: Pos s -> Pos s
    addN = over _Wrapped (+ noiseVal)
  in
    (addN x, addN y)

addPerlinNoisePath
  :: RandomGen g
  => g
  -> SVG_Path
  -> SVG_Path
addPerlinNoisePath gen =
  let
    seed = fst $ Rnd.randomR (2,5) gen
  in
    -- @fusing@ is probably hella overkill here, but why not.
    over (fusing (_Wrapped . traverse . SVGT._PathComm . _1))
    ( (SVGT._MoveTo . _Wrapped %~ addNoise seed)
    . (SVGT._LineTo . _Wrapped %~ addNoise seed)
    )

addPerlinNoiseToPoly
  :: Seed
  -> Poly
  -> Poly
addPerlinNoiseToPoly seed =
  over (_Wrapped . _2) (
    (SVGT.svg_polygon_start %~ addNoise seed)
  . (SVGT.svg_polygon_path . traverse %~ addNoise seed)
  )

makePolyProps
  :: Poly
  -> Map Text Text
makePolyProps poly =
  SVGT.makePolygonProps (poly ^. _Wrapped . _2)
  <> "class" =: "basic-poly"
  <> case poly ^. to unPoly . _1 . to unPolyAttrs of
    (Stroke, c) -> "stroke" =: toColour c <> "fill" =: "none"
    (Fill, c)   -> "fill" =: toColour c
  where
    toColour Greenish  = "lightseagreen"
    toColour Redish    = "palevioletred"
    toColour Orangeish = "orange"

makePolyAnimate
  :: Poly
  -> (Map BasicInner (Map Text Text))
makePolyAnimate poly =
  let
    toStr = fold . pts

    pts :: Poly -> Maybe Text
    pts p = p ^. _Wrapped . _2 . to SVGT.makePolygonProps . at "points"
  in
    Animate =: Map.fromList
           [ ("attributeName", "points")
           , ("to", toStr poly)
           , ("dur", "3s")
           , ("repeatCount", "1")
           , ("fill","freeze")
           , ("begin", "2s")
           ]

body :: StdGen -> UTCTime -> Widget x ()
body sGen _ = do

  eGenerate <- button "Generate"
  ePokePerlin <- button "New Noise"

  let
    tmpSeed   = 4
    sqPadding = Padding 3
    sqCount   = Count 600
    world     = World wheight wwidth
    size      = Size 1.5

    dSvgRootAttrs = pure $
      bgProps <> SVGT.makeSVGProps svgEl

    genPolys = generatePolys
      world size sqPadding sqCount

  rec (dPolys, dGen) <- splitDynPure <$> holdDyn (genPolys sGen)
        (genPolys <$> current dGen <@ eGenerate)

  let
    getRndNum :: RandomGen g => g -> (Int,g)
    getRndNum = Rnd.randomR (2,5)

  dRandInt <- foldDyn ($) (1,sGen) $ mergeWith (.)
    [ (getRndNum . snd) <$ eGenerate
    , (getRndNum . snd) <$ ePokePerlin
    ]

  let
    dPerlin = addPerlinNoiseToPoly . fst <$> dRandInt

  -- dNoiseAnim <- holdDyn (const mempty) $ leftmost
  --   [ makePolyAnimate <$ ePokePerlin
  --   , const mempty <$ eGenerate
  --   ]

  void . SVG.svgElDynAttr' SVG.SVG_Root dSvgRootAttrs $
    simpleList (NE.toList <$> dPolys) $ (\dPoly ->
      SVG.svgBasicDyn SVG.Polygon makePolyProps dPoly $
        -- dNoiseAnim <*> (dPerlin <*> dPoly)
        fmap makePolyAnimate $ dPerlin <*> dPoly
      )

head' :: StaticWidget x ()
head' = do
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: static @"css/genart.css") blank
  el "title" $ text "Oh my"

frontend
  :: StdGen
  -> UTCTime
  -> (StaticWidget x (), Widget x ())
frontend sGen t =
  ( head'
  , body sGen t
  )
