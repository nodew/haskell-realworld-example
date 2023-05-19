{-# LANGUAGE DeriveGeneric #-}

module Conduit.Api.Common where

import Conduit.Core.User
import Conduit.Util
import Data.Aeson
import RIO

newtype UserData a = UserData {userData :: a}
    deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (UserData a) where
    toJSON (UserData a) = object ["user" .= a]

instance FromJSON a => FromJSON (UserData a) where
    parseJSON = withObject "user" $ \o -> do
        a <- o .: "user"
        return (UserData a)

newtype Profile a = Profile {profile :: a}
    deriving (Show, Generic)

instance ToJSON a => ToJSON (Profile a) where
    toJSON (Profile a) = object ["profile" .= a]

instance FromJSON a => FromJSON (Profile a) where
    parseJSON = withObject "profile" $ \o -> do
        a <- o .: "profile"
        return (Profile a)

data UserProfile = UserProfile
    { profileUsername :: Text
    , profileBio :: Text
    , profileImage :: Text
    , profileFollowing :: Bool
    }
    deriving (Eq, Show, Generic)

instance ToJSON UserProfile where
    toJSON = genericToJSON $ toJsonOptions 7

instance FromJSON UserProfile where
    parseJSON = genericParseJSON $ toJsonOptions 7

mapUserToUserProfile :: User -> Bool -> UserProfile
mapUserToUserProfile user following =
    UserProfile
        { profileUsername = getUsername $ userName user
        , profileBio = userBio user
        , profileImage = userImage user
        , profileFollowing = following
        }
