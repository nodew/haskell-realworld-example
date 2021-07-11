module Conduit.JWT where

import RIO hiding ((^?), (^.))
import Control.Monad.Except (runExceptT)
import Crypto.JWT
    ( JWK,
      bestJWSAlg,
      ClaimsSet,
      JWTError,
      decodeCompact,
      encodeCompact,
      newJWSHeader,
      claimAud,
      claimExp,
      claimIat,
      claimIss,
      claimSub,
      defaultJWTValidationSettings,
      emptyClaimsSet,
      signClaims,
      string,
      verifyClaims,
      Audience(Audience),
      NumericDate(NumericDate) )
import Control.Lens
import Data.Time.Clock ( getCurrentTime, nominalDay, addUTCTime )
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8 )

import Conduit.Core.User ( Username(getUsername) )

mkClaims :: Username -> IO ClaimsSet
mkClaims name = do
    currentTime <- getCurrentTime
    let expiredAt = addUTCTime nominalDay currentTime
    pure $ emptyClaimsSet
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