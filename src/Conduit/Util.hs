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

flipM :: Monad m => m a -> (a -> b -> m c) -> b -> m c
flipM ma f arg = ma >>= flip f arg

flipM2 :: Monad m => m a -> (a -> b -> c -> m d) -> b -> c -> m d
flipM2 ma f b c = ma >>= \a -> f a b c

flipM3 :: Monad m => m a -> (a -> b -> c -> d -> m e) -> b -> c -> d -> m e
flipM3 ma f b c d = ma >>= \a -> f a b c d
