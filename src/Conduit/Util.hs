{-# LANGUAGE RankNTypes #-}
module Conduit.Util where

import RIO
import Control.Monad.Trans.Maybe ( MaybeT(..) )
import Data.Aeson ( defaultOptions, Options(fieldLabelModifier) )
import Data.List ( head, tail )
import Data.Char ( toLower )
import Data.UUID
import System.Random

toJsonOptions :: Int -> Options
toJsonOptions prefixLength =
    defaultOptions
        { fieldLabelModifier = headToLower . drop prefixLength }
    where
        headToLower x = toLower (head x) : tail x

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

newUUID :: IO UUID
newUUID = randomIO

-- whenJust :: Monad m => Maybe t -> (t -> m (Maybe a)) -> m (Maybe a)
-- whenJust mb f = maybe (return Nothing) f mb

flipMaybe :: Maybe a -> b -> (a -> b) -> b
flipMaybe mb error f = maybe error f mb
