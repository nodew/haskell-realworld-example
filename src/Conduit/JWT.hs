module Conduit.JWT where

import Conduit.Core.User (Username (getUsername))
import Control.Lens
import Control.Monad.Except (runExceptT)
import Crypto.JWT
    ( Audience (Audience)
    , ClaimsSet
    , JWK
    , JWTError
    , NumericDate (NumericDate)
    , bestJWSAlg
    , claimAud
    , claimExp
    , claimIat
    , claimIss
    , claimSub
    , decodeCompact
    , defaultJWTValidationSettings
    , emptyClaimsSet
    , encodeCompact
    , newJWSHeader
    , signClaims
    , string
    , verifyClaims
    )
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (addUTCTime, getCurrentTime, nominalDay)
import RIO hiding ((^.), (^?))

mkClaims :: Username -> IO ClaimsSet
mkClaims name = do
    currentTime <- getCurrentTime
    let expiredAt = addUTCTime nominalDay currentTime
    pure $
        emptyClaimsSet
            & claimIss ?~ "conduit-server"
            & claimAud ?~ Audience ["conduit-client"]
            & claimIat ?~ NumericDate currentTime
            & claimExp ?~ NumericDate expiredAt
            & claimSub ?~ (fromString . T.unpack . getUsername) name

signJwt :: JWK -> ClaimsSet -> IO (Either JWTError Text)
signJwt jwk claims = runExceptT $ do
    alg <- bestJWSAlg jwk
    decodeUtf8 . toStrict . encodeCompact <$> signClaims jwk (newJWSHeader ((), alg)) claims

verifyJwt :: JWK -> ByteString -> IO (Either JWTError ClaimsSet)
verifyJwt key token = runExceptT $ do
    let audCheck = (== "conduit-client")
    signedJwt <- decodeCompact $ fromStrict token
    verifyClaims (defaultJWTValidationSettings audCheck) key signedJwt

getSubject :: ClaimsSet -> Maybe Text
getSubject claimSet = fromMaybe "" (claimSet ^. claimSub) ^? string
