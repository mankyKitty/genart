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

import           Control.Lens                (from, fusing, over, to, under,
                                              ( # ), (%~), (^.), _1, _Wrapped)
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

import           Numeric.Noise               (Seed)
import qualified Numeric.Noise.Perlin        as Perlin

import qualified Reflex                      as R
import           Reflex.Dom.Core

import qualified Reflex.Dom.CanvasDyn        as Canvas

import           Reflex.Dom.Widget.SVG       (BasicSVG (Path))
import qualified Reflex.Dom.Widget.SVG       as SVG

import           Reflex.Dom.Widget.SVG.Types (Height (..), Pos, SVG_El (..),
                                              SVG_Path, SVG_Polygon, Width (..),
                                              X, Y, _PosX, _PosY)
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

strokeAttrs = Map.fromList
  [ ("stroke", textColour Col.black)
  , ("stroke-width", "0.2px")
  , ("fill", "none")
  ]

generatePath
  :: RandomGen g
  => World
  -> Size
  -> Padding
  -> Count
  -> g
  -> (SVG_Path, g)
generatePath wrld sz padding count =
  Rnd.runRand (runReaderT (createPath sz padding count) wrld)

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
    over (_Wrapped . traverse . SVGT._PathComm . _1)
    (\cmd -> cmd
      & SVGT._MoveTo . _Wrapped %~ addNoise seed
      & SVGT._LineTo . _Wrapped %~ addNoise seed
    )

body :: StdGen -> Widget x ()
body sGen = do
  eGenerate <- button "Generate"

  let
    sqPadding = Padding 3
    sqCount = Count 600
    world = World wheight wwidth 2
    size = Size 1.5

    dSvgRootAttrs = pure $
      bgProps <> SVGT.makeSVGProps svgEl

    genPath = generatePath
      world size sqPadding sqCount

  rec (dPath, dGen) <- splitDynPure <$> holdDyn (genPath sGen)
        (genPath <$> current dGen <@ eGenerate)

  dPerlinPath <- holdDyn (addPerlinNoisePath sGen)
    $ addPerlinNoisePath <$> current dGen <@ eGenerate

  void . SVG.svgElDynAttr' SVG.SVG_Root dSvgRootAttrs $
    SVG.svgBasicDyn_ SVG.Path (mappend strokeAttrs . SVGT.makePathProps) (dPerlinPath <*> dPath)

frontend :: StdGen -> (StaticWidget x (), Widget x ())
frontend sGen =
  ( el "title" $ text "Oh my"
  , body sGen
  )
