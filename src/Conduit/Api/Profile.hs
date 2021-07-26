{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Conduit.Api.Profile where

import RIO
import Data.Aeson
import Servant
import qualified Data.Text as T
import Data.Maybe

import Conduit.App
import Conduit.Core.User
import Conduit.Api.Common
import qualified Conduit.Db.User as UserDb

type ProfileApi = AuthProtect "Optional"
                    :> "profiles" :> Capture "username" Text
                    :> Get '[JSON] (Profile UserProfile)
             :<|> AuthProtect "Required"
                    :> "profiles" :> Capture "username" Text :> "follow"
                    :> Post '[JSON] (Profile UserProfile)
             :<|> AuthProtect "Required"
                    :> "profiles" :> Capture "username" Text :> "follow"
                    :> Delete '[JSON] (Profile UserProfile)

getProfileHandler :: Maybe User -> Text -> AppM (Profile UserProfile)
getProfileHandler maybeUser targetUsername
    | T.null targetUsername = throwIO err404
    | maybe "" (getUsername . userName) maybeUser == targetUsername = do
        let user = fromJust maybeUser
        return $ Profile $ mapUserToUserProfile user False
    | otherwise = do
        targetUser <- UserDb.getUserByName (Username targetUsername)
        case targetUser of
            Nothing -> throwIO err404
            Just targetUser' -> do
                following <- case maybeUser of
                                Nothing -> return False
                                Just user -> UserDb.checkFollowship user (userId targetUser')
                return $ Profile $ mapUserToUserProfile targetUser' following

followUserHandler :: User -> Text -> AppM (Profile UserProfile)
followUserHandler user targetUsername
    | T.null targetUsername = throwIO err404
    | (getUsername . userName) user == targetUsername = throwIO err403
    | otherwise = do
        targetUser <- UserDb.getUserByName (Username targetUsername)
        case targetUser of
            Nothing -> throwIO err404
            Just targetUser' -> do
                _ <- UserDb.followUser user (userId targetUser')
                return $ Profile $ mapUserToUserProfile targetUser' True

unFollowUserHandler :: User -> Text -> AppM (Profile UserProfile)
unFollowUserHandler user targetUsername
    | T.null targetUsername = throwIO err404
    | (getUsername . userName) user == targetUsername = throwIO err403
    | otherwise = do
        targetUser <- UserDb.getUserByName (Username targetUsername)
        case targetUser of
            Nothing -> throwIO err404
            Just targetUser' -> do
                _ <- UserDb.unfollowUser user (userId targetUser')
                return $ Profile $ mapUserToUserProfile targetUser' False

profileServer :: ServerT ProfileApi AppM
profileServer = getProfileHandler
            :<|> followUserHandler
            :<|> unFollowUserHandler
