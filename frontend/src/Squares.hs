{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ViewPatterns              #-}
module Squares
  ( World (..)
  , HasWorld (..)
  , Count (..)
  , Size (..)
  , Padding (..)
  , createManyPolys
  ) where

import           Control.Applicative                  (liftA2)
import           Control.Lens                         (Getter, from, makeClassy,
                                                       re, to, under, views,
                                                       ( # ), (^.), _Wrapped)

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

import           Reflex.Dom.Widget.SVG.Types          (Height, Pos, Width, X, Y, SVG_Polygon (..),
                                                       _PosX, _PosY)

data World = World
  { _worldHeight :: Height
  , _worldWidth  :: Width
  , _worldPerlinSeed :: Int
  }
makeClassy ''World

newtype Count = Count { unCount :: Int }
  deriving (Eq, Show)

newtype Size = Size { unSize :: Double }
  deriving (Eq, Show)

newtype Padding = Padding { unPadding :: Word8 }
  deriving (Eq, Show)

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

addToPos :: Float -> Pos p -> Pos p
addToPos n = under (from _Wrapped) (+n)

createPoly
  :: Size
  -> V2 Double
  -> SVG_Polygon
createPoly (realToFrac . unSize -> size) start =
  let
    startX = start ^. _x . toPosX
    startY = start ^. _y . toPosY

    x1 = addToPos size startX
    y1 = addToPos size startY

    rest =
      (x1, startY) :| [(x1, y1) , (startX, y1) , (startX, startY)]
  in
    SVG_Polygon (startX, startY) rest

createManyPolys
  :: ( MonadRandom m
     , MonadReader r m
     , HasWorld r
     )
  => Size
  -> Padding
  -> Count
  -> m [SVG_Polygon]
createManyPolys sqSize pad n = do
  let
    pad' = unPadding $ pad
    toBnd x = (pad', (floor x) `div` 2 - pad')

  wBnd <- views (worldWidth . _Wrapped) toBnd
  hBnd <- views (worldHeight . _Wrapped) toBnd

  let
    toDbl x y = V2 (fromIntegral x * 2) (fromIntegral y * 2)

    mk = liftA2 toDbl (Rnd.getRandomR wBnd) (Rnd.getRandomR hBnd)

  fmap nub . replicateM (fromIntegral . unCount $ n) $ createPoly sqSize <$> mk
