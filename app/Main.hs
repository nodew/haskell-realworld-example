module Main where

import Conduit
import Conduit.Config
import RIO

main :: IO ()
main = do
    config <- loadConfigFromEnv
    runConduit config
