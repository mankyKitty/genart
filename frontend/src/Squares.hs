{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}
module Squares
  ( World (..)
  , HasWorld (..)
  , Count (..)
  , Size (..)
  , Padding (..)
  , Poly (..)
  , PolyAttrs (..)
  , StrokeFill (..)
  , Colour (..)
  , createPath
  , createPolygons
  ) where

import           Control.Applicative                  (liftA2)
import           Control.Lens                         (Getter, from, makeClassy,
                                                       makeWrapped, re, to,
                                                       under, views, ( # ),
                                                       (^.), _Wrapped)

import           Control.Monad                        (replicateM)
import           Control.Monad.Reader                 (MonadReader)

import           Control.Monad.Random                 (MonadRandom)
import qualified Control.Monad.Random                 as Rnd

import           Data.Semigroup                       (sconcat, (<>))

import           Data.List.NonEmpty                   (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                   as NE

import           Data.List                            (nub)
import           Data.Word                            (Word8)

import           Linear.V2                            (V2 (V2), _x, _y)

import           Reflex.Dom.Widget.SVG.Types          (Height, Pos,
                                                       SVG_Path (..),
                                                       SVG_Polygon (..), Width,
                                                       X, Y, _PosX, _PosY)
import qualified Reflex.Dom.Widget.SVG.Types.SVG_Path as P

data World = World
  { _worldHeight :: Height
  , _worldWidth  :: Width
  }
makeClassy ''World

newtype Count = Count { unCount :: Int }
  deriving (Eq, Show)

newtype Size = Size { unSize :: Double }
  deriving (Eq, Show)

newtype Padding = Padding { unPadding :: Word8 }
  deriving (Eq, Show)

data StrokeFill
  = Stroke
  | Fill
  deriving (Eq, Show)

data Colour
  = Greenish
  | Orangeish
  | Redish
  deriving (Show, Eq)

newtype PolyAttrs = PolyAttrs
  { unPolyAttrs :: (StrokeFill, Colour) }
  deriving (Eq, Show)

newtype Poly = Poly
  { unPoly :: (PolyAttrs, SVG_Polygon) }
  deriving (Eq,Show)

toPosX :: Real a => Getter a (Pos X)
toPosX = to realToFrac . re _PosX

toPosY :: Real a => Getter a (Pos Y)
toPosY = to realToFrac . re _PosY

zeroX :: Pos X
zeroX = _PosX # 0

zeroY :: Pos Y
zeroY = _PosY # 0

negatePos :: Pos p -> Pos p
negatePos = under (from _Wrapped) negate

addToPos :: Real a => a -> Pos p -> Pos p
addToPos n = under (from _Wrapped) (+ realToFrac n)

createPositions
  :: Size
  -> Float
  -> Float
  -> (Pos X, Pos Y, Pos X, Pos Y)
createPositions (realToFrac . unSize -> sz) x y =
  ( _PosX # (x * 2)
  , _PosY # (y * 2)
  , _PosX # (sz + x * 2)
  , _PosY # (sz + y * 2)
  )

makeSquarePath
  :: Size
  -> Float
  -> Float
  -> SVG_Path
makeSquarePath sz x y =
  let
    (sX,sY,sPlusX,sPlusY) = createPositions sz x y
  in
    D $ P._M sX sY :|
    [ P._L sPlusX sY            -- a > b
    , P._L sPlusX sPlusY        -- b > c
    , P._L sX sPlusY            -- c > d
    , P._L sX sY                -- d > a
    ]

createShape
  :: ( MonadRandom m
     , MonadReader r m
     , HasWorld r
     , Real n
     , Eq s
     )
  => Size
  -> Padding
  -> Count
  -> (n -> n -> s)
  -> m (NonEmpty s)
createShape sz pad n f = do
  let
    pad' = unPadding pad
    toBnd x = (pad', floor x `div` 2 - pad')

    rndNum = fmap fromIntegral . Rnd.getRandomR

  wBnd <- views (worldWidth . _Wrapped) toBnd
  hBnd <- views (worldHeight . _Wrapped) toBnd

  let mk = f <$> rndNum wBnd <*> rndNum hBnd

  fmap NE.nub . (:|)
    <$> mk
    <*> replicateM (unCount n) mk

createPath
  :: ( MonadRandom m
     , MonadReader r m
     , HasWorld r
     )
  => Size
  -> Padding
  -> Count
  -> m SVG_Path
createPath sz pad n =
  sconcat <$> createShape sz pad n (makeSquarePath sz)

createPolygons
  :: ( MonadRandom m
     , MonadReader r m
     , HasWorld r
     )
  => Size
  -> Padding
  -> Count
  -> m (NonEmpty Poly)
createPolygons sz pad n =
  let
    attrs = fmap PolyAttrs $ (,)
      <$> Rnd.weighted [ (Stroke, 0.4), (Fill, 0.6) ]
      <*> Rnd.uniform [ Greenish, Redish, Orangeish ]

    mkPoly x y =
      let
        (sX,sY,sPlusX,sPlusY) = createPositions sz x y
      in
        SVG_Polygon (sX,sY)
        $ (sPlusX, sY)
        :| [ (sPlusX, sPlusY)
           , (sX, sPlusY)
           , (sX, sY)
           ]
  in
    traverse (fmap Poly . liftA2 (,) attrs . pure) =<<
      createShape sz pad n mkPoly

makeWrapped ''PolyAttrs
makeWrapped ''Poly
