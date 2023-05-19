{-# LANGUAGE RankNTypes #-}

module Conduit.Util where

import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson (Options (fieldLabelModifier), defaultOptions)
import Data.ByteArray (Bytes, convert)
import Data.Char (toLower)
import Data.List (head, tail)
import Data.Text.Encoding (decodeUtf8)
import Data.UUID
import RIO
import System.IO (hPutStrLn)
import System.Random

toJsonOptions :: Int -> Options
toJsonOptions prefixLength =
    defaultOptions
        { fieldLabelModifier = headToLower . drop prefixLength
        }
    where
        headToLower x = toLower (head x) : tail x

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

newUUID :: IO UUID
newUUID = randomIO

flipMaybe :: Maybe a -> b -> (a -> b) -> b
flipMaybe mb error f = maybe error f mb

fromTextToBytes :: Text -> Bytes
fromTextToBytes = convert . encodeUtf8

fromBytesToText :: Bytes -> Text
fromBytesToText = decodeUtf8 . convert

exitWithErrorMessageAndCode :: String -> ExitCode -> IO a
exitWithErrorMessageAndCode str e = hPutStrLn stderr str >> exitWith e

exitWithErrorMessage :: String -> IO a
exitWithErrorMessage msg = exitWithErrorMessageAndCode msg (ExitFailure 2)
