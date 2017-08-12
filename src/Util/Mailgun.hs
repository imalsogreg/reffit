{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE OverloadedStrings#-}

module Util.Mailgun where

------------------------------------------------------------------------------
import           Control.Lens         hiding ((.=))
import           Crypto.Random        (getSystemDRG, randomBytesGenerate)
import           Crypto.KDF.Scrypt    (generate, Parameters(..))
import           Crypto.Cipher.AES    (AES256)
import           Crypto.Cipher.Types  (BlockCipher(..), Cipher(..),nullIV)
import           Crypto.Error         (throwCryptoError)
import qualified Data.Aeson           as A
import qualified Data.Aeson.Types     as A
import           Data.Aeson           ((.:), (.=))
import           Data.Monoid
import           Data.Time
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString      as BS
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           GHC.Generics
import           GHC.Word
import qualified Network.HTTP.Client  as C
import qualified Network.HTTP.Client.TLS  as C
import qualified Network.HTTP.Types   as C
import qualified Network.Wreq         as W
import qualified Codec.Serialise      as S

------------------------------------------------------------------------------
data ResetRequest = ResetRequest
  { resetReqExpires :: UTCTime
  , resetReqEmail   :: T.Text
  } deriving (Eq, Show, Ord, Generic)

instance A.ToJSON   ResetRequest
instance A.FromJSON ResetRequest

instance S.Serialise ResetRequest

newtype SigningKey = SigningKey ByteString
    deriving (Eq)

instance Show SigningKey where
    show _ = "SigningKey \"secretbytestring\""


toSafeBlob :: S.Serialise a => SigningKey -> a -> BS.ByteString
toSafeBlob sk a =
    B64U.encode . encrypt sk . BSL.toStrict $ S.serialise a

fromSafeBlob :: S.Serialise a => SigningKey -> BS.ByteString -> Either S.DeserialiseFailure a
fromSafeBlob sk blob =
    S.deserialiseOrFail . BSL.fromStrict . decrypt sk $ B64U.decodeLenient blob


mailtest :: IO ()
mailtest = do
    m <- C.newTlsManager
    n <- getCurrentTime
    let rr = ResetRequest n "imalsogreg@gmail.com"
    (sk,mk) <- deriveFromFile "../signing-key.txt"
    sendMailgunResetRequest rr m sk mk
    return ()

sendMailgunResetRequest
    :: ResetRequest
    -> C.Manager
    -> SigningKey
    -> T.Text
    -> IO (Maybe T.Text)
sendMailgunResetRequest rr mgr sk mgKey = do
    let requestObj = [ "from"    W.:= ("no-reply@mg.reffit.com" :: T.Text)
                     , "to"      W.:= resetReqEmail rr
                     , "subject" W.:= ("Reffit password reset request" :: T.Text)
                     , "text"    W.:= bodyText
                     , "html"    W.:= bodyHtml
                     ]
        resetToken = T.decodeUtf8 $ toSafeBlob sk rr
        emailLink = "https://reffit.com/reset/" <> resetToken
        bodyText = T.unlines
          ["We received a password request for the account at reffit.com"
          , "If you did not request a password reset, please ignore this email."
          , "To reset your password, visit this link:"
          , emailLink
          , "The link will be valid for one day."
          , "Thanks, happy Reffiting!"
          ]
        bodyHtml = T.unlines
          ["<!DOCTYPE html><html><body><h3>Please reset your reffit password</h3>"
          , "<p>"
          , "We received a password request for the account at reffit.com"
          , "If you did not request a password reset, please ignore this email."
          , "</p><a href=\"" <> emailLink <> "\">Click Here</a> to reset your"
            <> "password, or follow the link below: <br/>"
          , emailLink
          , "<p>Thanks, happy Reffiting!</p></body></html>"
          ]
        mgUrl = "https://api:" <> mgKey <> "@api.mailgun.net/v3/mg.reffit.com/messages"

    let opts = W.defaults --  & W.param "Content-Type" .~ ["application/json"]
    W.postWith opts (T.unpack mgUrl) (requestObj :: [W.FormParam])
    -- request  <- C.parseRequest (T.unpack mgUrl)
    -- print request
    -- response <- flip C.httpLbs mgr $ C.setRequestHeader "Content-Type" "application/json" $ request
    --     { C.method = "POST"
    --     , C.requestBody = C.RequestBodyLBS $ A.encode requestObj}
    -- print (show $ C.statusCode $ C.responseStatus response)
    return Nothing

------------------------------------------------------------------------------
-- This encryption code is mostly copied from
-- https://stackoverflow.com/questions/42456724/how-to-use-haskell-cryptonite

encrypt :: SigningKey -> ByteString -> ByteString
encrypt (SigningKey key) plainData = ctrCombine ctx nullIV plainData
  where ctx :: AES256
        ctx = throwCryptoError $ cipherInit key

decrypt :: SigningKey -> ByteString -> ByteString
decrypt = encrypt

--Scrypt KDF
deriveKey :: T.Text -> ByteString -> ByteString
deriveKey password salt = generate params (T.encodeUtf8 password) salt
  where params =
            Parameters { n = paramN , r = paramR , p = paramP
                       , outputLength = paramKeyLen}

deriveFromFile :: FilePath -> IO (SigningKey, T.Text)
deriveFromFile fp = do
    Just bs <- fmap A.decode $ BSL.readFile fp
    case A.parseMaybe secretParams bs of
        Nothing -> undefined
        Just (passwd,salt,mg) ->
            return (SigningKey $ deriveKey passwd (T.encodeUtf8 salt), mg)
  where
      secretParams = A.withObject "SecretParams" $ \v -> (,,)
        <$> v .: "password"
        <*> v .: "salt"
        <*> v .: "mailgun"

saltSize :: Int
saltSize = 32

paramN :: Word64
paramN = 16

paramR, paramP, paramKeyLen :: Int
paramR = 8
paramP = 1
paramKeyLen = 32
