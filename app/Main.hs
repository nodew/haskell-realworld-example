module Main where

import RIO

import Conduit
import Conduit.Config

main :: IO ()
main = do
    config <- loadConfigFromEnv
    runConduit config
