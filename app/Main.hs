module Main where

import RIO
import Dhall

import Conduit
import Conduit.Config

main :: IO ()
main = do
    config <- input auto "./conduit.dhall"
    runConduit config
