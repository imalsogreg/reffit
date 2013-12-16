{-# LANGUAGE OverloadedStrings #-}

module HandleViewUser (handleViewUser) where

import Reffit.Types
import Reffit.AcidTypes

import Safe
import Control.Applicative ((<$>),(<*>),pure)
import qualified Data.List as List
import qualified Data.Map as Map
import Snap.Core (getParam)
import Snap.Types (writeText)
import Snap.Snaplet (Handler)
import Snap.Snaplet.AcidState (query)
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth 
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Int
import Control.Lens
import Control.Monad

handleViewUser :: Handler App (AuthManager App) ()
handleViewUser = do
  userMap <- query QueryAllUsers
  cAUser' <- currentUser
  profileName' <- getParam "username" 
  case decodeUtf8 <$> profileName' of 
    Nothing -> writeText "Error decoding username"  --TODO
    Just profileName -> case Map.lookup profileName userMap of 
      Nothing -> writeText "User not in database."  -- TODO 
      Just profileUser -> do
        let cUser' = join $ Map.lookup <$> (userLogin <$> cAUser') <*> pure userMap :: Maybe User
        renderWithSplices "user" (profileSplices cUser' profileUser) 

profileSplices :: Maybe User -> User -> Splices (SnapletISplice App)
profileSplices cUser' profileUser = do
  "userName"         ## I.textSplice $ userName profileUser
  "followButtonText" ## I.textSplice followBtnText
  where followBtnText =
          case elem (userName profileUser) <$> (userFollowing <$> cUser') of
            Just False -> "Follow"
            Just True  -> "Unfollow"
            Nothing    -> "n/a"
  