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
import Reffit.User
import Reffit.DataVersion
import Reffit.FieldTag

import Safe
import Control.Applicative ((<$>),(<*>),pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import Control.Lens (makeLenses, view,over)
import Data.Time
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Text as T hiding (head)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import Data.Typeable (Typeable)
import Data.List
import qualified Data.Set as Set
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
  , _fieldTags  :: FieldTags
  } deriving (Show, Generic, Typeable)
  
makeLenses ''PersistentState

deriveSafeCopy scv 'base ''PersistentState

queryAllDocs :: Query PersistentState (Map.Map DocumentId Document)
queryAllDocs = asks _documents

-- TODO: addDocument, addComment, and addCritique all have
-- the newId t = hash t <|> length docs <|> firstNotTaken...
-- Factor this out.

-- TODO: Check that document title isn't already taken
addDocument :: Maybe User -> Document -> Update PersistentState ()
addDocument user' doc = do  -- HandleNewPaper now finds a good Id
  modify (over documents (Map.insert (docId doc) doc))
  case user' of
    Just user -> 
      modify (over users     (Map.insert (userName user)
                              user { userHistory = PostedDocument (docId doc)
                                                   : (userHistory user) }))
    Nothing -> return ()

addSummary :: Maybe User -> DocumentId -> Summary
              -> Update PersistentState (Maybe SummaryId)
addSummary user' pId summary = do  
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
        case user' of
          Just user ->
            modify (over users (Map.insert (userName user)
                                (user { userHistory = WroteSummary (docId doc) sId :
                                                      (userHistory user) })))
          Nothing -> return ()
        return (Just sId)
        where
          sId = head . filter (\k -> Map.notMember k (docSummaries doc)) $
                (sHash:sInd:sAll)
          sHash = fromIntegral . hash . summaryProse $ summary
          sInd  = fromIntegral . Map.size $ docSummaries doc
          sAll  = [0..]

castSummaryVote :: User -> Bool -> DocumentId -> Document
                   -> SummaryId -> Summary -> UpDownVote -> UTCTime
                   -> Update PersistentState ()
castSummaryVote user isAnon dId doc sId summary voteVal t = do
  modify (over users $ \us' ->
           let vRecord = if isAnon then Nothing else Just voteVal
               histItem = VotedOnSummary dId sId vRecord t
               u' = user { userHistory = histItem : userHistory user }
           in Map.insert (userName user) u' us')
  modify (over documents $ \ds ->
           let s' = summary { summaryVotes = voteVal : summaryVotes summary }
               d' = doc { docSummaries = Map.insert sId s' (docSummaries doc)}
           in Map.insert dId d' ds) 
          
castCritiqueVote :: User -> Bool -> DocumentId -> Document
                 -> CritiqueId -> Critique -> UpDownVote -> UTCTime
                 -> Update PersistentState ()
castCritiqueVote user isAnon dId doc cId critique voteVal t = do
  modify (over users $ \us' ->
           let vRecord = if isAnon then Nothing else Just voteVal
               histItem = VotedOnCritique dId cId vRecord t
               u' = user { userHistory = histItem : userHistory user }
           in Map.insert (userName user) u' us')
  modify (over documents $ \ds ->
           let c' = critique { critiqueReactions = voteVal : critiqueReactions critique }
               d' = doc { docCritiques = Map.insert cId c' (docCritiques doc) } 
           in Map.insert dId d' ds)

addCritique :: Maybe User -> DocumentId -> Critique 
               -> Update PersistentState (Maybe SummaryId)
addCritique user' pId critique = do
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
      case user' of
        Just user -> 
          modify (over users $ Map.insert (userName user)
                  (user { userHistory = WroteCritique pId cId :
                                        (userHistory user)}))
        Nothing -> return ()
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
-- username.  But seems safer to check and report this
addUser :: UserName -> T.Text -> UTCTime -> Update PersistentState ()
addUser uName email t = do
  allUsers <- gets _users
  case Map.lookup uName allUsers of
    Nothing ->
      modify (over users ( Map.insert uName $ User uName email Set.empty Set.empty [] Set.empty Set.empty t ))
    Just _ -> do  -- This checks and refuses to overwrite, but silently
      modify (over users id) 

userFollow :: User -> User -> UTCTime -> Update PersistentState ()
userFollow a b t = do
  let a' = a { userFollowing  = Set.insert (userName b) (userFollowing a)
             , userHistory    = FollowedUser (userName b) t : (userHistory a)}
      b' = b { userFollowedBy = Set.insert (userName a) (userFollowedBy b) }
  modify (over users $
          \u0 ->  Map.insert (userName a') a' $
                  Map.insert (userName b') b' u0)

userUnfollow :: User -> User -> Update PersistentState ()
userUnfollow a b = do
  let a' = a { userFollowing  = Set.delete (userName b) (userFollowing a) }
      b' = b { userFollowedBy = Set.delete (userName a) (userFollowedBy b)}
  modify (over users $
          \u0 -> Map.insert (userName a') a' $
                 Map.insert (userName b') b' u0) 

pin :: User -> DocumentId -> Bool -> UTCTime -> Update PersistentState ()
pin user dId doPin t = do
  let board' board0 = case doPin of
        True  -> Set.insert dId board0
        False -> Set.delete dId board0
  modify (over users $
          \u0 -> Map.insert (userName user)
                 (user { userPinboard = board' (userPinboard user)
                       , userHistory = PinnedDoc dId t : (userHistory user)})
                 u0)

queryAllDocClasses :: Query PersistentState [DocClass]
queryAllDocClasses = asks _docClasses

addDocClass :: DocClass -> Update PersistentState ()
addDocClass dc = do
  modify (over docClasses (dc:))
  
queryAllFieldTags :: Query PersistentState FieldTags
queryAllFieldTags = asks _fieldTags

addUserTag :: User -> TagPath -> Update PersistentState ()
addUserTag user tp =
  modify (over users (Map.insert (userName user)
                      (user { userTags = Set.insert tp (userTags user) })))

deleteUserTag :: User -> TagPath -> Update PersistentState ()
deleteUserTag user tp =
  modify (over users (Map.insert (userName user)
                      (user {userTags = Set.delete tp (userTags user) } )))

addFieldTag :: TagPath -> Update PersistentState ()
addFieldTag tp = modify (over fieldTags (insertTag tp))

makeAcidic ''PersistentState ['addDocument,         'queryAllDocs
                             , 'queryAllUsers,      'addUser
                             , 'addUserTag,         'deleteUserTag
                             , 'userFollow,         'userUnfollow
                             , 'pin
                             , 'queryAllDocClasses, 'addDocClass
                             , 'queryAllFieldTags,  'addFieldTag
                             , 'addSummary,         'addCritique
                             , 'castSummaryVote,    'castCritiqueVote]