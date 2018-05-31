{-# LANGUAGE RankNTypes #-}
module Backend where

import Common.Api
import Frontend

import qualified System.Random                    as Rnd

import qualified Obelisk.Backend as Ob

backend :: IO ()
backend = do
  sGen <- Rnd.getStdGen
  Ob.backend Ob.def
    { Ob._backendConfig_head = fst (frontend sGen)
    }
  -- where
  --   f :: forall x. StaticWidget x ()
  --   f = fst frontend
