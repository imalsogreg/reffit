{-# LANGUAGE NoMonomorphismRestriction #-}

module Reffit.Handlers.Helpers where

import Reffit.Types
import Reffit.AcidTypes
import Reffit.Document
import Reffit.User

import Snap.Core
import Control.Applicative ((<$>),(<*>),pure)
import qualified Data.Map as Map
import Snap.Snaplet (Handler)
import Snap.Snaplet.AcidState (query)

import Snap.Snaplet.Auth
import Application
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding (decodeUtf8)

import Control.Monad
import Safe
import Data.Maybe

-- |Return the Reffet User corresponding to the logged in auth user
-- or Nothing if (not logged on | user can't be found in db)
currentReffitUser :: Handler App (AuthManager App) (Maybe User)
currentReffitUser = do
  userMap <- query QueryAllUsers
  cUser   <- currentUser
  return . join $ Map.lookup <$> (userLogin <$> cUser) <*> pure userMap

-- |(Acid-State) Get a Document from the database by its DocumentId
lookupDoc :: DocumentId -> Handler App (AuthManager App) (Maybe Document)
lookupDoc dId = do
  docMap <- query QueryAllDocs
  return $ Map.lookup dId docMap

getTextParam :: MonadSnap m => BS.ByteString -> m (Maybe T.Text)
getTextParam name = do
  pText <- fmap (map decodeUtf8 . BS.words) <$> getParam name
  return . join $ (headMay <$> pText)

getTextParams :: MonadSnap m => BS.ByteString -> m [T.Text]
getTextParams name = do
  pText <- fmap (map decodeUtf8 . BS.words) <$> getParam name
  case pText of
    Nothing -> return []
    Just ts -> return ts

getIntegralParam :: (MonadSnap m, Integral r, Read r) => BS.ByteString -> m (Maybe r)
getIntegralParam name = do
  pText <- fmap (map (readMay . BS.unpack) . BS.words) <$> getParam name
  return . join . join $ (headMay <$> pText)

getIntegerParam :: (MonadSnap m) => BS.ByteString -> m (Maybe Integer)
getIntegerParam name = do
  pText <- fmap (map (readMay . BS.unpack) . BS.words) <$> getParam name
  return . join . join $ (headMay <$> pText)

getIntegerParams :: (MonadSnap m) => BS.ByteString -> m [Integer]
getIntegerParams name = do
  pText <- fmap (map(readMay . BS.unpack) . BS.words) <$> getParam name
  case pText of
    Nothing -> return []
    Just ns -> return $ catMaybes ns
