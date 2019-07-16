module Main where

import qualified Lib
import Network.Wai.Handler.Warp (run)

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  port <- read <$> fromMaybe "8080" <$> lookupEnv "port"
  putStrLn $ "Running a server " ++ (show port)
  run port Lib.app
