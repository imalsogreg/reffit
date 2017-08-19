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
import Reffit.OverviewComment
import Reffit.Document
import Reffit.Discussion
import Reffit.User
import Reffit.FieldTag

import Control.Monad.Reader (asks)
import Control.Monad.State  (gets, modify)
import Data.Aeson
import Data.Time
import Data.SafeCopy
import qualified Data.Text as T hiding (head)
import GHC.Generics
import Data.Typeable (Typeable)
import qualified Data.Set as Set
import Data.Hashable
import qualified Data.Map as Map
import Data.Serialize
import Snap.Snaplet.AcidState (Update, Query,makeAcidic)
import Control.Lens
import GHC.Int
import qualified Data.Tree as Tree

data PersistentState = PersistentState {
    _documents  :: Map.Map DocumentId Document
  , _users      :: Map.Map UserName User
  , _docClasses :: [DocClass]
  , _fieldTags  :: FieldTags
  } deriving (Show, Read, Generic, Typeable)
makeLenses ''PersistentState
deriveSafeCopy 0 'base ''PersistentState

instance Serialize PersistentState where
instance ToJSON PersistentState where
instance FromJSON PersistentState where

queryAllDocs :: Query PersistentState (Map.Map DocumentId Document)
queryAllDocs = asks _documents

-- TODO - what's the right way here?  Can't be this
updateAllDocs :: Map.Map DocumentId Document -> Update PersistentState ()
updateAllDocs docMap = modify (over documents (const docMap ))

-- TODO: addDocument, addComment, and addCritique all have
-- the newId t = hash t <|> length docs <|> firstNotTaken...
-- Factor this out.  Now use this.
getNewId :: T.Text -> [Int32] -> Int32
getNewId elementText usedIds =
  head . filter (`notElem` usedIds) $
  (cHash:cInd:cAll)
  where
    cHash = abs . fromIntegral . hash $ T.unpack elementText
    cInd  = fromIntegral . length $ usedIds
    cAll = [0..]

-- TODO: Check that document title isn't already taken
addDocument :: Maybe User -> Document -> Update PersistentState ()
addDocument user' doc = do  -- HandleNewPaper now finds a good Id
  modify (over documents (Map.insert (docId doc) doc))
  case user' of
    Just user ->
      modify (over users (Map.insert (userName user)
                          user { userHistory = PostedDocument (docId doc)
                                               : (userHistory user) }))
    Nothing -> return ()

addDocumentDiscussionPoint :: DiscussionPoint -> Maybe DiscussionPointId ->
                              Document -> Update PersistentState ()
addDocumentDiscussionPoint dp parent' doc =
  modify (over documents (Map.insert (docId doc)
                          (doc {docDiscussion = insertAt dp' parent' (docDiscussion doc)})))
  where
    oldIds = map _dID $ concatMap Tree.flatten (docDiscussion doc)
    dp' = dp { _dID = getNewId (_dText dp) oldIds
             , _dContext = ( docId doc, Nothing, parent' )
             }

addCommentDiscussionPoint :: DiscussionPoint -> Maybe DiscussionPointId ->
                             Document -> OverviewCommentId -> OverviewComment ->
                             Update PersistentState ()
addCommentDiscussionPoint dp parent' doc commentId comment =
  modify (over documents (Map.insert (docId doc)
                          (doc {docOComments = Map.insert commentId comment'
                                              (docOComments doc)})))
    where
      oldIds = map _dID $ concatMap Tree.flatten (ocDiscussion comment)
      dp' = dp { _dID = getNewId (_dText dp) oldIds
               , _dContext = ( docId doc, Nothing, parent' )
               }
      comment' = comment { ocDiscussion = insertAt dp' parent' (ocDiscussion comment) }


addOComment :: Maybe User -> DocumentId -> OverviewComment
            -> Maybe OverviewCommentId -> Update PersistentState (Maybe OverviewCommentId)
addOComment user' pId comment cId' = do
  docs <- gets _documents
  case Map.lookup pId docs of
    Nothing -> return Nothing -- TODO - how to signal error?
    Just doc -> do
      modify (over documents $ \docs' ->
               (Map.insert (docId doc)
                (doc { docOComments = Map.insert cId comment (docOComments doc) }) docs'))
      case user' of
        Just user ->
          modify (over users (Map.insert (userName user)
                              (user {userHistory = WroteOComment (docId doc) cId : userHistory user })))
        Nothing -> return ()
      return (Just cId)
      where
        cId = case cId' of 
          Just n -> n
          Nothing -> head . filter (\k -> Map.notMember k (docOComments doc)) $
                     (cHash:cInd:cAll)
        cHash = abs . fromIntegral . hash $ T.unpack (ocText comment) ++
                show (ocPostTime comment)
        cInd  = fromIntegral . Map.size $ docOComments doc
        cAll = [0..]

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
                  -- TODO - POSSIBLE BUG, I'm replaying a function
                  -- that assigns an ID, through schema migration.
                  -- will the ID change?
                  (doc { docOComments =
                            Map.insert sId (migrate $ summToOComment summary)
                            (docOComments doc)})
                  docs'))
        case user' of
          Just user ->
            -- TODO POSSIBLE BUG (same as other POSSIBLE BUGS)
            modify (over users
                    (Map.insert (userName user)
                     (user { userHistory =
                                migrate (WroteSummary0 (docId doc) sId)
                                :(userHistory user) })))
          Nothing -> return ()
        return (Just sId)
        where
          sId = head . filter (\k -> Map.notMember k (docOComments doc)) $
                (sHash:sInd:sAll)
          sHash = fromIntegral . hash . summaryProse $ summary
          sInd  = fromIntegral . Map.size $ docOComments doc
          sAll  = [0..]




castSummaryVote :: User -> Bool -> DocumentId -> Document
                   -> SummaryId -> Summary -> UpDownVote -> UTCTime
                   -> Update PersistentState ()
castSummaryVote user isAnon dId doc sId summary voteVal t = do
  modify (over users $ \us' ->
           let vRecord = if isAnon then Nothing else Just voteVal
               histItem = migrate (VotedOnSummary0 dId sId vRecord t) -- NOTE Added migrate here
               u' = user { userHistory = histItem : userHistory user }
           in Map.insert (userName user) u' us')
  modify (over documents $ \ds ->
           let s' = summary { summaryVotes = voteVal : summaryVotes summary }
               d' = doc { docOComments = Map.insert sId
                                         (migrate $ summToOComment s')
                                         (docOComments doc)}
           in Map.insert dId d' ds)

castOCommentVote :: User -> Bool -> DocumentId -> Document
                 -> OverviewCommentId -> OverviewComment -> UpDownVote
                 -> UTCTime
                 -> Update PersistentState ()

castOCommentVote user isAnon dId doc cId comment voteVal t = do
  modify (over users $ \us' ->
           let vRecord = if isAnon then Nothing else Just voteVal
               histItem = VotedOnOComment dId cId vRecord t
               u' = user { userHistory = histItem : userHistory user }
           in Map.insert (userName user) u' us')
  modify (over documents $ \ds ->
           let c' = comment { ocResponse = voteVal : ocResponse comment }
               d' = doc {docOComments = Map.insert cId c' (docOComments doc) }
           in Map.insert dId d' ds)


castCritiqueVote :: User -> Bool -> DocumentId -> Document
                 -> CritiqueId -> Critique -> UpDownVote -> UTCTime
                 -> Update PersistentState ()
castCritiqueVote user isAnon dId doc cId critique voteVal t = do
  modify (over users $ \us' ->
           let vRecord = if isAnon then Nothing else Just voteVal
               histItem = migrate $ VotedOnCritique0 dId cId vRecord t
               u' = user { userHistory = histItem : userHistory user }
           in Map.insert (userName user) u' us')
    -- TODO Here I'm casting Critique into Ocomment - it seems like this
    -- might change the comment id? POSSIBLE BUG
  modify (over documents $ \ds ->
           let c' = critique { critiqueReactions =
                                  voteVal : critiqueReactions critique }
               c'' = migrate $ critToOComment c'
               d' = doc { docOComments =
                             Map.insert cId c'' (docOComments doc) }
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
                (doc {docOComments =
                         Map.insert cId (migrate $ critToOComment critique)
                         (docOComments doc)}) -- BACK HERE
--                (doc { docCritiques0 = Map.insert cId critique
--                                       (docCritiques0 doc)})

                docs'))
      case user' of
        Just user ->
          modify (over users $ Map.insert (userName user)
                  (user { userHistory = migrate (WroteCritique0 pId cId) :
                                        (userHistory user)}))
        Nothing -> return ()
      return (Just cId)
        where
          cId = head . filter (\k -> Map.notMember k (docOComments doc)) $
                (cHash:cInd:cAll)
          cHash = fromIntegral . hash . critiqueProse $ critique
          --cInd  = fromIntegral . Map.size $ docCritiques0 doc
          -- TODO POSSIBLE BUG: will replaying these functions in acid-state
          -- lead to changed critique ID's??
          cInd = fromIntegral . Map.size $ docOComments doc
          cAll  = [0..]


queryAllUsers :: Query PersistentState (Map.Map T.Text User)
queryAllUsers = asks _users

-- TODO : find right way here
updateAllUsers :: Map.Map UserName User -> Update PersistentState ()
updateAllUsers us = modify (over users (const us))

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

-- TODO: right type, wrong combinator
updateAllDocClasses :: [DocClass] -> Update PersistentState ()
updateAllDocClasses classes = modify (over docClasses (const classes))

addDocClass :: DocClass -> Update PersistentState ()
addDocClass dc = do
  modify (over docClasses (dc:))

queryAllFieldTags :: Query PersistentState FieldTags
queryAllFieldTags = asks _fieldTags

-- TODO: find appropriate combinator
updateAllFieldTags :: FieldTags -> Update PersistentState ()
updateAllFieldTags tags = modify (over fieldTags (const tags))

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

makeAcidic ''PersistentState ['addDocument,         'queryAllDocs, 'updateAllDocs
                             , 'queryAllUsers,      'addUser,      'updateAllUsers
                             , 'addUserTag,         'deleteUserTag
                             , 'userFollow,         'userUnfollow
                             , 'pin
                             , 'addOComment,        'castOCommentVote
                             , 'queryAllDocClasses, 'addDocClass, 'updateAllDocClasses
                             , 'queryAllFieldTags,  'addFieldTag, 'updateAllFieldTags
                             , 'addSummary,         'addCritique
                             , 'castSummaryVote,    'castCritiqueVote
                             , 'addDocumentDiscussionPoint
                             , 'addCommentDiscussionPoint]
