{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Conduit.Config where

import Dhall (FromDhall)
import Hasql.Connection
import GHC.Generics
import RIO (Text, Show, Word16, toStrictBytes, ($))
import RIO.Text

data Config = Config
    { cfgPort :: Word16 
    , cfgJwtSecret :: Text
    , cfgDb :: DbConfig
    } deriving (Generic, Show, FromDhall)

data DbConfig = DbConfig 
    { dbPort     :: Word16
    , dbHost     :: Text 
    , dbUser     :: Text
    , dbPasswd   :: Text
    , dbDatabase :: Text  
    } deriving (Generic, Show, FromDhall)

mapDbConfigToSettings :: DbConfig -> Settings
mapDbConfigToSettings cfg =
    settings host port user password database
    where
        host = encodeUtf8 $ dbHost cfg
        port = dbPort cfg
        user = encodeUtf8 $ dbUser cfg  
        password = encodeUtf8 $ dbPasswd cfg
        database = encodeUtf8 $ dbDatabase cfg
