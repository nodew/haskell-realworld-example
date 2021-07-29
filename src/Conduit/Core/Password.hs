{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Conduit.Core.Password where

import RIO
import Rel8 ( DBEq, DBType )
import Data.Aeson ( FromJSON, ToJSON )
import Crypto.Random ( MonadRandom(getRandomBytes) )
import Crypto.Hash ( hashWith, SHA256(SHA256) )
import qualified Data.Text as T

newtype Salt = Salt { getSalt :: Text }
    deriving newtype (Eq, Show, Read, DBEq, DBType)

newtype HashedPassword = HashedPassword { getHashedPasswd :: Text }
    deriving newtype (Eq, Show, Read, DBEq, DBType)

newtype Password = Password { getPassword :: Text }
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

newSalt :: MonadIO m => m Salt
newSalt = do
    rnd <- liftIO $ getRandomBytes 32
    return $ Salt $ T.pack $ show $ hashWith SHA256 (rnd :: ByteString)

hashPasswordWithSalt :: Password -> Salt -> HashedPassword
hashPasswordWithSalt (Password password) (Salt salt) =
    let _salt = hashWith SHA256 (encodeUtf8 salt)
        _password = hashWith SHA256 _salt
    in HashedPassword $ T.pack $ show _password

hashPassword :: MonadIO m => Password -> m (HashedPassword, Salt)
hashPassword password = do
    salt <- newSalt
    let hash = hashPasswordWithSalt password salt
    return (hash, salt)

verifyPassword :: Password -> Salt -> HashedPassword -> Bool
verifyPassword password salt hash =
    let _hash = hashPasswordWithSalt password salt
    in hash == _hash
