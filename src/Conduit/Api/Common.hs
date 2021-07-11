{-# LANGUAGE DeriveGeneric #-}
module Conduit.Api.Common where

import RIO
import Data.Aeson

import Conduit.Util
import Conduit.Core.User

newtype UserData a = UserData { userData :: a }
    deriving (Show, Generic)

instance ToJSON a => ToJSON (UserData a) where
    toJSON (UserData a) = object ["user" .= a]

instance FromJSON a => FromJSON (UserData a) where
    parseJSON = withObject "user" $ \o -> do
        a <- o .: "user"
        return (UserData a)

newtype Profile a = Profile { profile :: a }
    deriving (Show, Generic)

instance ToJSON a => ToJSON (Profile a) where
    toJSON (Profile a) = object ["profile" .= a]

instance FromJSON a => FromJSON (Profile a) where
    parseJSON = withObject "profile" $ \o -> do
        a <- o .: "profile"
        return (Profile a)

data UserProfile = UserProfile 
    { profileUsername  :: Text
    , profileBio       :: Text
    , profileImage     :: Text
    , profileFollowing :: Bool
    } deriving (Show, Generic)

instance ToJSON UserProfile where
    toJSON = genericToJSON $ toJsonOptions 7

mapUserToUserProfile :: User -> Bool -> UserProfile
mapUserToUserProfile user following = UserProfile
    { profileUsername  = getUsername $ userName user
    , profileBio       = userBio user
    , profileImage     = userImage user
    , profileFollowing = following
    }