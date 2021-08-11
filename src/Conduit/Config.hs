{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Conduit.Config where

import RIO
import Dhall
import Hasql.Connection
import qualified Data.Text as T
import Data.Char (toLower)

data Config = Config
    { cfgPort :: Word16
    , cfgJwtSecret :: Text
    , cfgDb :: DbConfig
    } deriving (Generic, Show)

instance FromDhall Config where
    autoWith _ = genericAutoWith $ mkDhallInterpretOptions 3

data DbConfig = DbConfig
    { dbPort     :: Word16
    , dbHost     :: Text
    , dbUser     :: Text
    , dbPasswd   :: Text
    , dbDatabase :: Text
    , dbPoolSize :: Word16
    } deriving (Generic, Show)

instance FromDhall DbConfig where
    autoWith _ = genericAutoWith $ mkDhallInterpretOptions 2

mkDhallInterpretOptions :: Int -> InterpretOptions
mkDhallInterpretOptions prefixLength = defaultInterpretOptions { fieldModifier = headToLower . T.drop prefixLength }
    where
        headToLower x = T.pack [toLower $ T.head x] `T.append` T.tail x

mapDbConfigToSettings :: DbConfig -> Settings
mapDbConfigToSettings cfg =
    settings host port user password database
    where
        host = encodeUtf8 $ dbHost cfg
        port = dbPort cfg
        user = encodeUtf8 $ dbUser cfg
        password = encodeUtf8 $ dbPasswd cfg
        database = encodeUtf8 $ dbDatabase cfg
