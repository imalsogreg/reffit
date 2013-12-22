{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Reffit.Types where

import           Data.Text
import qualified Data.Map as Map
import           GHC.Int
import           GHC.Generics
import           Data.Typeable
import           Reffit.DataVersion
import           Reffit.FieldTag
import           Data.Time.Clock
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Set as Set


data DocClass = DocClass { docClassName :: Text
                         } deriving (Show, Generic, Typeable, Eq)
deriveSafeCopy scv 'base ''DocClass

type UserName   = Text
type CritiqueId = Int32
type SummaryId  = Int32
type DocumentId = Int32

data UpDownVote = DownVote | UpVote
                deriving (Show, Eq, Ord, Generic, Typeable)
deriveSafeCopy scv 'base ''UpDownVote

data UserEvent = WroteCritique   DocumentId CritiqueId
               | VotedOnCritique DocumentId CritiqueId (Maybe UpDownVote) UTCTime
               | WroteSummary    DocumentId SummaryId
               | VotedOnSummary  DocumentId SummaryId (Maybe UpDownVote) UTCTime
               | PostedDocument  DocumentId
               | FollowedUser    UserName   UTCTime
               | PinnedDoc       DocumentId UTCTime
               deriving (Show, Eq, Ord, Generic, Typeable)
deriveSafeCopy scv 'base ''UserEvent

data User = User { userName       :: UserName
                 , userEmail      :: Text
                 , userFollowing  :: Set.Set UserName
                 , userFollowedBy :: Set.Set UserName
                 , userHistory    :: [UserEvent]
                 , userPinboard   :: Set.Set DocumentId
                 , userTags       :: Set.Set TagPath
                 , userJoinTime   :: UTCTime
                 } deriving (Show, Eq, Ord, Generic,Typeable)
deriveSafeCopy scv 'base ''User

data Summary = Summary { summaryPoster   :: Maybe UserName
                       , summaryProse    :: Text
                       , summaryVotes    :: [UpDownVote]
                       , summaryPostTime :: UTCTime
                       } deriving (Show, Generic)
deriveSafeCopy scv 'base ''Summary

data QualityDim = Novelty | Rigor | Coolness
                deriving (Eq, Show, Generic, Typeable)
deriveSafeCopy scv 'base ''QualityDim

data Critique = Critique { critiqueProse     :: Text
                         , critiquePoster    :: Maybe UserName
                         , critiqueDim       :: QualityDim
                         , critiqueVal       :: UpDownVote
                         , critiqueReactions :: [UpDownVote]
                         , critiquePostTime  :: UTCTime
                         } deriving (Show, Generic)
deriveSafeCopy scv 'base ''Critique


data Document = Document { docUploader  :: Maybe UserName
                         , docId        :: DocumentId
                         , docTitle     :: Text
                         , docAuthors   :: [Text]
                         , docLink      :: Text
                         , docClass     :: DocClass
                         , docFieldTags :: [TagPath]
                         , docSummaries :: Map.Map SummaryId  Summary
                         , docCritiques :: Map.Map CritiqueId Critique
                         , docPostTime  :: UTCTime
                         } deriving (Show, Generic, Typeable)
deriveSafeCopy scv 'base ''Document

data DocumentHints = DocumentHints { titleHint   :: Text
                                   , authorsHint :: [Text]
                                   , linkHint    :: Text
                                   , yearHint    :: Maybe Int
                                   }