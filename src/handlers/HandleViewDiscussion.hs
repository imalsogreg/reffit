{-# LANGUAGE OverloadedStrings #-}

module HandleViewDiscussion (handleViewDiscussion) where

import Reffit.Types
import Reffit.User
import Reffit.Discussion
import Reffit.Document
import Reffit.AcidTypes

import Snap.Core
import Snap.Snaplet (Handler)
import Snap.Snaplet.Auth
import Application
import Snap.Snaplet.AcidState (query)
import Control.Lens
import Data.Time
import Control.Monad (join)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))
import Data.Text.Encoding (decodeUtf8)
import Safe
import qualified Data.Text as T

handleViewDiscussion :: Handler App (AuthManager App) ()
handleViewDiscussion = do
  us    <- query QueryAllUsers
  pId'  <- getParam "paperid"
  ocId' <- getParam "commentid"
  aUser <- currentUser
  t     <- liftIO $ getCurrentTime
  let u = join $ (flip Map.lookup) us <$> userLogin <$> aUser :: Maybe User
  case join $ readMay. T.unpack . decodeUtf8 <$> pId' of
    Nothing -> writeText "Need paperid parameter"
    Just pId -> do
      let comment = join $ readMay . T.unpack . decodeUtf8 <$> ocId'
      
      
