{-# LANGUAGE OverloadedStrings #-}

module HandleSummaryVote(
  handleSummaryVote
  )
where

import           Reffit.Types
import           Reffit.AcidTypes

import           Safe
import           Application 
import           Snap.Snaplet.AcidState (Update, Query, Acid,
                                         HasAcid (getAcidStore),
                                         makeAcidic,
                                         update,query,acidInit)
import           Snap.Core
import           Snap.Snaplet(Handler)
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Control.Applicative
import           Data.Monoid
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding (decodeUtf8)
import           Text.Digestive
import           Text.Digestive.Blaze.Html5
import           Heist 
import qualified Heist.Interpreted            as I
import           Application
import qualified Text.Blaze.Html5             as H
import           Text.Digestive.Snap (runForm)
import           Text.Digestive.Heist  
import qualified Data.ByteString.Char8        as BS

handleSummaryVote :: UpDownVote -> Handler App (AuthManager App) ()  
handleSummaryVote voteDir = do 
  userMap   <- query QueryAllUsers
  docs      <- query QueryAllDocs
  ft        <- query QueryAllFieldTags
  pId'      <- getParam "paperid"
  sId'      <- getParam "summaryid"
  authUser' <- currentUser
  -- TODO extremely deep nesting - should I be in ErrorT here?
  case (,) ($) readMay . T.unpack . decodeUtf8 <*> pId' <*> sId' of  
    Nothing -> writeText "paperid/summaryid formatting error" --TODO proper error message
    Just (Just (pId,sId)) ->
      case Map.lookup pId docs of
        Nothing -> writeText "paperid not in database"
        Just doc -> 
          case Map.lookup sId (docSummaries doc) of
            Nothing -> writeText "summaryId not in database"
            Just summary ->
              case (Map.lookup <$> (userLogin <$> authUser') <*> pure userMap) of
                Nothing -> writeText "Need to log in."
                Just (Just user) -> do
                  
          