{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Conduit.Core.Password where

import RIO
import Rel8 ( DBEq, DBType )
import Data.Aeson ( FromJSON, ToJSON )
import Crypto.Random ( MonadRandom(getRandomBytes) )
import Crypto.Hash ( hashWith, SHA256(SHA256) )
import Crypto.KDF.BCrypt as Bcrypt (bcrypt, validatePassword)
import qualified Data.Text as T
import Data.ByteArray (Bytes, convert)
import Conduit.Util (fromTextToBytes, fromBytesToText)

newtype Salt = Salt Bytes

newtype HashedPassword = HashedPassword { getHashedPasswd :: Text }
    deriving newtype (Eq, Show, Read, DBEq, DBType)

newtype Password = Password { getPassword :: Text }
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

unsafePassword :: Text -> Password
unsafePassword = Password

mkPassword :: Text -> Maybe Password
mkPassword rawText =
    if T.length rawText <= 6 then Nothing
    else Just $ Password rawText

newSalt :: MonadIO m => m Salt
newSalt = liftIO $ Salt <$> getRandomBytes 16

hashPasswordWithSalt :: Password -> Salt -> HashedPassword
hashPasswordWithSalt (Password password) (Salt salt) =
    let hash = Bcrypt.bcrypt 10 salt (fromTextToBytes password)
    in HashedPassword $ fromBytesToText hash

hashPassword :: MonadIO m => Password -> m HashedPassword
hashPassword password = hashPasswordWithSalt password <$> newSalt

verifyPassword :: Password -> HashedPassword -> Bool
verifyPassword (Password password) (HashedPassword hash) = Bcrypt.validatePassword (fromTextToBytes password) (fromTextToBytes hash)
