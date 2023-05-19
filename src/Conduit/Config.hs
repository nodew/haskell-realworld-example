{-# LANGUAGE DeriveGeneric #-}

module Conduit.Config where

import Conduit.Util (exitWithErrorMessage)
import Data.Char (toLower)
import RIO
import System.Environment (getEnv)

data Config = Config
    { cfgPort :: Int
    , cfgJwk :: Text
    , cfgConnectString :: ByteString
    , cfgPoolSize :: Int
    }
    deriving (Generic, Show)

loadConfigFromEnv :: IO Config
loadConfigFromEnv = do
    port <- getEnv' "8080" "APP_PORT"
    jwk <- getEnv' "" "JWK_STRING"
    connectString <- getEnv' "" "POSTGRES_CONNECT_STRING"
    poolSize <- getEnv' "1" "POSTGRES_POOL_SIZE"

    if null jwk
        then exitWithErrorMessage "Environment variable JWK_STRING is missing"
        else
            if null connectString
                then exitWithErrorMessage "Environment variable POSTGRES_CONNECT_STRING is missing"
                else
                    return $
                        Config
                            (readInt 8080 port)
                            (fromString jwk)
                            (fromString connectString)
                            (readInt 1 poolSize)

readInt :: Int -> String -> Int
readInt optional value = fromMaybe optional $ readMaybe value

getEnv' :: String -> String -> IO String
getEnv' optional key =
    catch (getEnv key) (const $ pure optional :: IOException -> IO String)
