{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------
-- acid-state code for Reffit
-----------------------------------

module Reffit.AcidTypes where

import Reffit.Types
import Reffit.DataVersion

import Safe
import Control.Applicative ((<$>),(<*>),pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import Control.Lens (makeLenses, view,over) 
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Text as T hiding (head)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import Data.Typeable (Typeable)
import Data.List
import GHC.Int
import Data.Hashable
import qualified Data.Map as Map
import Snap.Core (getParam)
import Snap.Util.FileServe (serveDirectory)
import Snap (SnapletInit, Snaplet, Handler,
             addRoutes, nestSnaplet, serveSnaplet,
             defaultConfig, makeSnaplet,
             snapletValue, writeText, modify, gets)
import Snap.Snaplet.AcidState (Update, Query, Acid, 
                                HasAcid (getAcidStore),
                                makeAcidic, update,
                                query, acidInit)

data PersistentState = PersistentState {
    _documents  :: Map.Map DocumentId Document
  , _users      :: Map.Map UserName User
  , _docClasses :: [DocClass]
  , _fieldTags  :: [FieldTag]
  } deriving (Show, Generic, Typeable)
  
makeLenses ''PersistentState

deriveSafeCopy scv 'base ''PersistentState

queryAllDocs :: Query PersistentState (Map.Map DocumentId Document)
queryAllDocs = asks _documents

-- TODO: addDocument, addComment, and addCritique all have
-- the newId t = hash t <|> length docs <|> firstNotTaken...
-- Factor this out.

-- TODO: Check that document title isn't already taken
addDocument :: Document -> Update PersistentState ()
addDocument doc = do
  oldDocs <- gets _documents
  let newDoc = doc { docId = newId }
      newId = head . filter (\k -> Map.notMember k oldDocs)
              $ (tHash: tLen: tNotTaken)
      tHash = fromIntegral . hash . docTitle $ doc:: Int32
      tLen  = fromIntegral (Map.size oldDocs)  :: Int32
      tNotTaken = [0..maxBound] :: [Int32]
  modify (over documents (Map.insert newId newDoc))

addSummary :: DocumentId -> Summary 
              -> Update PersistentState (Maybe SummaryId)
addSummary pId summary = do  
  docs <- gets _documents
  case Map.lookup pId docs of
    Nothing -> return Nothing 
               -- TODO - how to signal error?
    Just doc -> do
        modify (over documents $ \docs' ->
                 (Map.insert
                  (docId doc)
                  (doc { docSummaries = Map.insert sId summary 
                                        (docSummaries doc)}) 
                  docs')) 
        return (Just sId)
        where
          sId = head . filter (\k -> Map.notMember k (docSummaries doc)) $
                (sHash:sInd:sAll)
          sHash = fromIntegral . hash . summaryProse $ summary
          sInd  = fromIntegral . Map.size $ docSummaries doc
          sAll  = [0..]

castSummaryVote :: UserName -> Bool -> DocumentId
                   -> SummaryId -> UpDownVote
                   -> Update PersistentState ()
castSummaryVote uId isAnon dId sId voteVal = do
  us <- gets _users
  docs  <- gets _documents
  case Map.lookup dId docs of
    Nothing -> return () -- TODO Error page
    Just doc -> case Map.lookup sId (docSummaries doc) of
      Nothing -> return () --TODO error page
      Just summary -> case Map.lookup uId us of
        Nothing -> return () --TODO error page
        Just u  -> do
          modify (over users $ \us' ->
                   let vRecord = if isAnon then Nothing else Just voteVal
                       histItem = VotedOnSummary dId sId vRecord
                       u' = u { userHistory = histItem : userHistory u } :: User
                   in Map.insert uId u' us')
          modify (over documents $ \ds ->
                   let s' = summary { summaryVotes = voteVal : summaryVotes summary }
                       d' = doc { docSummaries = Map.insert sId s' (docSummaries doc)}
                   in Map.insert dId d' ds) 
          
castCritiqueVote :: User -> Bool -> DocumentId -> Document
                 -> CritiqueId -> Critique -> UpDownVote
                 -> Update PersistentState ()
castCritiqueVote user isAnon dId doc cId critique voteVal = do
  modify (over users $ \us' ->
           let vRecord = if isAnon then Nothing else Just voteVal
               histItem = VotedOnCritique dId cId vRecord
               u' = user { userHistory = histItem : userHistory user }
           in Map.insert (userName user) u' us')
  modify (over documents $ \ds ->
           let c' = critique { critiqueReactions = voteVal : critiqueReactions critique }
               d' = doc { docCritiques = Map.insert cId c' (docCritiques doc) } 
           in Map.insert dId d' ds)

addCritique :: DocumentId -> Critique 
               -> Update PersistentState (Maybe SummaryId)
addCritique pId critique = do
  docs <- gets _documents
  case Map.lookup pId docs of
    Nothing -> modify (over documents id) >> return Nothing
               -- TODO - how to signal an error?
    Just doc -> do
      modify (over documents $ \docs' ->
               (Map.insert
                (docId doc)
                (doc { docCritiques = Map.insert cId critique
                                      (docCritiques doc)})
                docs'))
      return (Just cId)
        where
          cId = head . filter (\k -> Map.notMember k (docCritiques doc)) $ 
                (cHash:cInd:cAll)
          cHash = fromIntegral . hash . critiqueProse $ critique
          cInd  = fromIntegral . Map.size $ docCritiques doc
          cAll  = [0..]

queryAllUsers :: Query PersistentState (Map.Map T.Text User)
queryAllUsers = asks _users

-- TODO - how can I alert the caller that there's already
-- a user by that name?
-- There SHOULDN'T be, because addUser should only get called
-- when a NEW user registers an account and gets an Auth
-- username.  But seems safer to check and report this assumption
addUser :: UserName -> Update PersistentState ()
addUser uName = do
  allUsers <- gets _users
  case Map.lookup uName allUsers of
    Nothing ->
      modify (over users ( Map.insert uName $ User uName [] [] []))
    Just _ -> do  -- This checks and refuses to overwrite, but silently
      modify (over users id)

queryAllDocClasses :: Query PersistentState [DocClass]
queryAllDocClasses = asks _docClasses

addDocClass :: DocClass -> Update PersistentState ()
addDocClass dc = do
  modify (over docClasses (dc:))
  
queryAllFieldTags :: Query PersistentState [FieldTag]
queryAllFieldTags = asks _fieldTags
 
addFieldTag :: FieldTag -> Update PersistentState ()
addFieldTag ft = modify (over fieldTags (ft:))

makeAcidic ''PersistentState ['addDocument, 'queryAllDocs
                             , 'queryAllUsers, 'addUser
                             , 'queryAllDocClasses, 'addDocClass
                             , 'queryAllFieldTags,  'addFieldTag
                             , 'addSummary, 'addCritique
                             , 'castSummaryVote, 'castCritiqueVote]