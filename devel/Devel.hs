module Devel where

import           Obelisk.Run

import           Backend
import           Frontend

import           Data.Time     (getCurrentTime)
import qualified System.Random as Rnd

main :: Int -> IO ()
main port = do
  sGen <- Rnd.getStdGen
  time <- getCurrentTime
  run port backend (frontend sGen time)
