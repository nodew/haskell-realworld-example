{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Conduit.Api.Profile where

import Conduit.Api.Common
import Conduit.App
import Conduit.Core.User
import qualified Conduit.Repository.User as UserRepository
import Conduit.Util
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import RIO
import Servant

type ProfileApi =
    AuthProtect "Optional"
        :> "profiles"
        :> Capture "username" Text
        :> Get '[JSON] (Profile UserProfile)
        :<|> AuthProtect "Required"
            :> "profiles"
            :> Capture "username" Text
            :> "follow"
            :> Post '[JSON] (Profile UserProfile)
        :<|> AuthProtect "Required"
            :> "profiles"
            :> Capture "username" Text
            :> "follow"
            :> Delete '[JSON] (Profile UserProfile)

getProfileHandler :: Maybe User -> Text -> AppM (Profile UserProfile)
getProfileHandler mbUser targetUsername
    | T.null targetUsername = throwIO err404
    | maybe "" (getUsername . userName) mbUser == targetUsername = do
        let user = fromJust mbUser
        return $ Profile $ mapUserToUserProfile user False
    | otherwise =
        UserRepository.getUserByName (Username targetUsername)
            >>= maybe
                (throwIO err404)
                ( \targetUser -> do
                    following <- flipMaybe mbUser (return False) $ \user -> UserRepository.checkFollowship user (userId targetUser)
                    return $ Profile $ mapUserToUserProfile targetUser following
                )

followUserHandler :: User -> Text -> AppM (Profile UserProfile)
followUserHandler user targetUsername
    | T.null targetUsername = throwIO err404
    | (getUsername . userName) user == targetUsername = throwIO err403
    | otherwise = do
        UserRepository.getUserByName (Username targetUsername)
            >>= maybe
                (throwIO err404)
                ( \targetUser -> do
                    _ <- UserRepository.followUser user (userId targetUser)
                    return $ Profile $ mapUserToUserProfile targetUser True
                )

unFollowUserHandler :: User -> Text -> AppM (Profile UserProfile)
unFollowUserHandler user targetUsername
    | T.null targetUsername = throwIO err404
    | (getUsername . userName) user == targetUsername = throwIO err403
    | otherwise =
        UserRepository.getUserByName (Username targetUsername)
            >>= maybe
                (throwIO err404)
                ( \targetUser -> do
                    _ <- UserRepository.unfollowUser user (userId targetUser)
                    return $ Profile $ mapUserToUserProfile targetUser False
                )

profileServer :: ServerT ProfileApi AppM
profileServer =
    getProfileHandler
        :<|> followUserHandler
        :<|> unFollowUserHandler
