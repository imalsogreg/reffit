{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators       #-}

module Reffit.CrossRef where

import           Reffit.Document
import           Reffit.Types

-- import           Application
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Error
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Aeson                 ((.:))
import qualified Data.Aeson                 as A
import           Data.Aeson.Lens
import qualified Data.Aeson.Types           as A
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Char                  as C
import           Data.IORef
import qualified Data.List                  as L
import           Data.LruCache
import           Data.LruCache.IO
import           Data.Maybe                 (catMaybes, isJust, listToMaybe)
import           Data.Proxy
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Data.Time
import           GHC.Generics
import           Network.Browser            hiding (Proxy (..))
import qualified Network.HTTP               as H
import qualified Network.HTTP.Client        as C
import qualified Network.HTTP.Client.TLS    as C
import           Network.URI
import           Safe
import           Servant.API
import           Servant.Client
import           Snap                       hiding (Headers (..), get)
import           Snap.Core                  (getParam)
import           Snap.Snaplet.Auth

data CrossRef = CrossRef
  { crManager             :: C.Manager
  , crCache               :: LruHandle T.Text DocumentHints
  , crRateLimitNextOkTime :: IORef UTCTime
  , crRateLimitOverride   :: Maybe Double
  , crAccessToken         :: MVar ()
  }

type CrossRefAPI = "works"
    :> Capture "doi" T.Text
    :> Get '[JSON] (Headers '[Header "X-Rate-Limit-Limit" Int] CrossRefSingleton)

api :: Proxy CrossRefAPI
api = Proxy

crossRef :: CrossRef -> T.Text -> IO (Either T.Text DocumentHints)
crossRef cr doi = do
    putStrLn $ "Trying " ++ T.unpack doi
    fmap (fmapL (\(e :: SomeException) -> T.pack $ show e)) $
                  try (cached (crCache cr) doi hitAPI)
    where
      hitAPI :: IO DocumentHints
      hitAPI = withMVar (crAccessToken cr) $ \() -> do
          now <- getCurrentTime
          tOk <- readIORef (crRateLimitNextOkTime cr)
          threadDelay . floor $ 1000000 * realToFrac (min 1 $ max 0 $ diffUTCTime tOk now)
          v <- runClientM (client api doi) (ClientEnv (crManager cr) apiBase Nothing)
          case v of
              Left e    -> error (show e)
              Right (Headers cref hs) -> do
                  case hs of
                      (HCons (Header rate) HNil) -> do
                          writeIORef (crRateLimitNextOkTime cr)
                                     (addUTCTime
                                      (realToFrac $ 1/ fromIntegral rate) tOk)
                          putStrLn $ "Wrote rate " ++ show rate
                      _ -> putStrLn "No rate"
                  return $ parseDocumentHints cref


initializeCrossRef :: IO CrossRef
initializeCrossRef = do
    tNow  <- newIORef =<< getCurrentTime
    token <- newMVar ()
    mgr   <- C.newTlsManager
    c <- newLruHandle 500
    return $ CrossRef mgr c tNow Nothing token


data DocumentHints = DocumentHints { titleHint   :: T.Text
                                   , authorsHint :: [T.Text]
                                   , linkHint    :: T.Text
                                   , yearHint    :: Maybe Int
                                   } deriving (Eq, Show, Generic)


instance A.ToJSON DocumentHints

parseDocumentHints :: CrossRefSingleton -> DocumentHints
parseDocumentHints (CrossRefSingleton msg _ _) = DocumentHints
  { titleHint   = fromMaybe (error "No CrossRef title") . listToMaybe $ m_title msg
  , authorsHint = (\(CrossRefAuthor giv fam) -> T.unwords [giv, fam]) <$> m_authors msg
  , yearHint    = (\(y,_,_) -> Just (fromIntegral y)) . toGregorian . unCrossRefDate $ m_created msg
  , linkHint    = m_URL msg
  }

data CrossRefSingleton = CrossRefSingleton
  { cMessage :: CrossRefMessage
  , cStatus  :: T.Text
  , cVersion :: T.Text
  } deriving (Eq, Ord, Show, Generic)

instance A.FromJSON CrossRefSingleton where
  parseJSON (A.Object o) = CrossRefSingleton
    <$> o .: "message"
    <*> o .: "status"
    <*> o .: "message-version"

data CrossRefMessage = CrossRefMessage
  { m_authors :: [CrossRefAuthor]
  , m_title   :: [T.Text]
  , m_URL     :: T.Text
  , m_subejct :: [T.Text]
  , m_created :: CrossRefDate
  } deriving (Eq, Ord, Show, Generic)

instance A.FromJSON CrossRefMessage where
    parseJSON (A.Object o) = CrossRefMessage
      <$> o .: "author"
      <*> o .: "title"
      <*> o .: "URL"
      <*> o .: "subject"
      <*> o .: "created"

data CrossRefAuthor = CrossRefAuthor
  { cra_given  :: T.Text
  , cra_family :: T.Text
  } deriving (Eq, Ord, Show)

instance A.FromJSON CrossRefAuthor where
    parseJSON (A.Object o) = CrossRefAuthor
        <$> o .: "given"
        <*> o .: "family"

data CrossRefDate = CrossRefDate { unCrossRefDate :: Day }
    deriving (Eq, Ord, Show)

instance A.FromJSON CrossRefDate where
    parseJSON (A.Object o) = CrossRefDate <$> do
        parts <- o .: "date-parts"
        case parts of
            ([y,m,d]:_) -> return $ fromGregorian (fromIntegral y) m d
            _           -> mzero
    parseJSON _ = mzero

apiBase :: BaseUrl
apiBase = BaseUrl Https "api.crossref.org" 443 ""

