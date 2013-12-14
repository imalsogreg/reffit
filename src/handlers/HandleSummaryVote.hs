{-# LANGUAGE OverloadedStrings #-}
 -- TODO this module needs to be renamed, we do both types of vote casting here
module HandleSummaryVote(
  handleSummaryVote,
  handleCritiqueVote
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
import           GHC.Int

-- TODO handle anonymity of votes
handleSummaryVote :: UpDownVote -> Handler App (AuthManager App) ()  
handleSummaryVote voteDir = do 
  userMap   <- query QueryAllUsers
  docs      <- query QueryAllDocs
  ft        <- query QueryAllFieldTags
  pId'      <- getParam "paperid"
  sId'      <- getParam "summaryid"
  authUser' <- currentUser
  -- TODO extremely deep nesting - should I be in ErrorT here?
  case  readMay . T.unpack . decodeUtf8 <$> pId' of   
    Nothing -> writeText "handleSummaryVote paperid formatting error" --TODO proper error message
    Just (Just pId) -> case readMay . T.unpack . decodeUtf8 <$> sId' of
      Nothing -> writeText $ T.concat ["summaryid formatting error. pId:", (T.pack . show $ pId')]
      Just (Just sId) -> 
        case Map.lookup pId docs of
          Nothing -> writeText "paperid not in database"
          Just doc -> 
            case Map.lookup sId (docSummaries doc) of
              Nothing -> writeText "summaryId not in database"
              Just summary ->
                case (Map.lookup <$> (userLogin <$> authUser') <*> pure userMap) of
                  Nothing -> writeText "Need to log in."
                  Just (Just u) -> do
                    update (CastSummaryVote (userName u) False pId sId voteDir)
                    writeText "Done casting vote."

-- TODO: Handle anonymity of votes
handleCritiqueVote :: UpDownVote -> Handler App (AuthManager App) ()
handleCritiqueVote voteDir = do
  userMap  <- query QueryAllUsers
  docs     <- query QueryAllDocs
  idParam' <- getParam "idParam"
  authUser' <- currentUser
  -- TODO so similar to handleSummaryVote - badly need a refactor
  case idParam' of
    Nothing -> writeText "idParam not found"
    Just idParam ->
      let (pId',cId') = (T.breakOn "." . decodeUtf8) $ idParam
          (pIdM,cIdM) = (readMay $ T.unpack pId', readMay .T.unpack . T.tail $ cId')
      in 
       case (pIdM, cIdM) of 
        (Nothing,Nothing) -> writeText $ T.concat ["paperid formatting error"
                                            ," idParam' :",  T.pack . show $ idParam'
                                            ,"  pId' :", T.pack . show $ pId'
                                            ,"  cId' IS :", T.pack . show $ T.tail cId'] 
        (Nothing,_) -> writeText "paperid formatting error"
        (_,Nothing) -> writeText "critiqueid formatting error here"
        (Just pId, Just cId) -> case Map.lookup pId docs of
            Nothing -> writeText "Document isn't in database"
            Just doc -> case Map.lookup cId (docCritiques doc) of
              Nothing -> writeText "critique isn't in database"
              Just critique ->
                case Map.lookup <$> (userLogin <$> authUser') <*> pure userMap of
                  Nothing -> writeText "Need to log in"
                  Just Nothing -> writeText "Just nothing - need to log in"
                  Just (Just u) -> do
                --TODO handle vote anonymity
                    update (CastCritiqueVote u False pId doc cId critique voteDir) 