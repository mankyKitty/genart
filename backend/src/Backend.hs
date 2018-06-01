{-# LANGUAGE RankNTypes #-}
module Backend where

import           Common.Api
import           Frontend

import           Data.Time       (getCurrentTime)
import qualified System.Random   as Rnd

import qualified Obelisk.Backend as Ob

backend :: IO ()
backend = do
  sGen <- Rnd.getStdGen
  time <- getCurrentTime
  Ob.backend Ob.def
    { Ob._backendConfig_head = fst (frontend sGen time)
    }
