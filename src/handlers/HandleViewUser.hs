{-# LANGUAGE OverloadedStrings #-}

module HandleViewUser (handleViewUser, handleFollow, handlePin) where

import Reffit.Types
import Reffit.AcidTypes
import PaperRoll

import Safe
import Control.Applicative ((<$>),(<*>),pure)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Snap.Core (getParam, redirect, writeBS)
import Snap.Types (writeText)
import Snap.Snaplet (Handler)
import Snap.Snaplet.AcidState (query,update)
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth 
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Int
import Control.Lens
import Control.Monad
import Data.Maybe

userPinboardDocs :: Map.Map DocumentId Document -> User -> [Document]
userPinboardDocs docs =
  catMaybes . map (\dId -> Map.lookup dId docs) . Set.toList . userPinboard

handleFollow :: Bool -> Handler App (AuthManager App) ()
handleFollow doFollow = do
  aUser <- currentUser
  us <- query QueryAllUsers
  fUser' <- getParam "username"
  case join $ ((\uName -> Map.lookup uName us) . userLogin) <$> aUser of
    Nothing -> redirect "#"
    Just thisUser -> case join $ ((\uName -> Map.lookup uName us) . decodeUtf8) <$> fUser' of
      Nothing -> writeBS "username Param error" -- TODO error page
      Just fUser -> let toPath = encodeUtf8 $ T.append "/user/" (userName fUser) in
        case doFollow of
             True  -> update (UserFollow   thisUser fUser) >>= \_ -> redirect toPath
             False -> update (UserUnfollow thisUser fUser) >>= \_ -> redirect toPath


handlePin :: Bool -> Handler App (AuthManager App) ()
handlePin doPin = do
  aUser <- currentUser
  us <- query QueryAllUsers
  dId'  <- getParam "paperid"
  case join $ ((\uName -> Map.lookup uName us) . userLogin) <$> aUser of 
    Nothing -> redirect "#"
    Just user -> case join $ (readMay . T.unpack . decodeUtf8) <$> dId' of
      Nothing -> writeBS "Param error" --TODO error page
      Just dId -> do
        _ <- update $ Pin user dId doPin
        redirect . BS.pack $ "/view_article/" ++ (show dId)
    
handleViewUser :: Handler App (AuthManager App) ()
handleViewUser = do
  userMap <- query QueryAllUsers
  docs    <- query QueryAllDocs
  cAUser' <- currentUser
  profileName' <- getParam "username" 
  case decodeUtf8 <$> profileName' of 
    Nothing -> writeText "Error decoding username"  --TODO
    Just profileName -> case Map.lookup profileName userMap of 
      Nothing -> writeText "User not in database."  -- TODO 
      Just profileUser -> do
        let cUser' = join $ Map.lookup <$> (userLogin <$> cAUser') <*> pure userMap :: Maybe User
        renderWithSplices "user" (profileSplices cUser' profileUser docs) 
 
profileSplices :: Maybe User -> User -> Map.Map DocumentId Document -> Splices (SnapletISplice App)
profileSplices cUser' profileUser docs = do
  -- Conditionally splice OUT the 'follow button'
  -- TODO this is a temporary measure to prevent self-following.
  when (cUser' == Just profileUser) $ "followButton" ## I.textSplice ""
  "userName"         ## I.textSplice $ userName profileUser
  "followBtnText" ## I.textSplice followBtnText
  "followBtnLink" ## I.textSplice followBtnLink
  "nPinboard"     ## I.textSplice . T.pack . show . 
    length $ userPinboardDocs docs profileUser
  "nFollowing"    ## I.textSplice . T.pack . show . Set.size . userFollowing
    $ profileUser
  "nFollowers"    ## I.textSplice . T.pack . show . Set.size . userFollowedBy
    $ profileUser
  (allPaperRollSplices $ userPinboardDocs docs profileUser)
  where (followBtnText,followBtnLink) =
          case Set.member (userName profileUser) <$> (userFollowing <$> cUser') of
            Just False -> ("Follow",   T.append "follow/"  (userName profileUser))
            Just True  -> ("Unfollow", T.append "unfollow/" (userName profileUser))
            Nothing    -> ("n/a","/")
  