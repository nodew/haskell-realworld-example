{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Conduit.Core.Password where

import RIO
import Rel8 ( DBEq, DBType )
import Data.Aeson ( FromJSON, ToJSON )
import Crypto.Random ( MonadRandom(getRandomBytes) )
import Crypto.Hash ( hashWith, SHA256(SHA256) )
import qualified Data.Text as T

type Salt = Text

newtype Password = Password { getPassword :: Text } 
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

newSalt :: MonadIO m => m Text 
newSalt = do
    rnd <- liftIO $ getRandomBytes 32
    return $ T.pack $ show $ hashWith SHA256 (rnd :: ByteString)

hashPasswordWithSalt :: Password -> Text -> Text
hashPasswordWithSalt (Password password) salt =
    let _salt = hashWith SHA256 (encodeUtf8 salt)
        _password = hashWith SHA256 _salt
    in T.pack $ show _password

hashPassword :: MonadIO m => Password -> m (Text, Text)
hashPassword password = do
    salt <- newSalt
    let hash = hashPasswordWithSalt password salt
    return (hash, salt)

verifyPassword :: Password -> Text -> Text -> Bool 
verifyPassword password salt hash = 
    let _hash = hashPasswordWithSalt password salt
    in hash == _hash