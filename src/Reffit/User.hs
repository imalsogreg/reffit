{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}

module Reffit.User where

import           Reffit.Types
import           Reffit.FieldTag

import qualified Data.Set as Set
import           Data.Text
import           Data.Time
import           GHC.Generics
import           Data.Typeable
import           Data.SafeCopy (base,extension,deriveSafeCopy,Migrate,MigrateFrom,migrate)

data UserEvent = WroteOComment   DocumentId OverviewCommentId
               | VotedOnOComment DocumentId OverviewCommentId (Maybe UpDownVote) UTCTime
               | PostedDocument  DocumentId
               | FollowedUser    UserName   UTCTime
               | PinnedDoc       DocumentId UTCTime
               deriving (Show, Eq, Ord, Generic, Typeable)
deriveSafeCopy 2 'extension ''UserEvent

data User = User { userName       :: UserName 
                 , userEmail      :: Text
                 , userFollowing  :: Set.Set UserName
                 , userFollowedBy :: Set.Set UserName
                 , userHistory    :: [UserEvent]
                 , userPinboard   :: Set.Set DocumentId
                 , userTags       :: Set.Set TagPath
                 , userJoinTime   :: UTCTime
                 } deriving (Show, Eq, Ord, Generic,Typeable)
deriveSafeCopy 1 'extension ''User


-- Started counting versions at 0.  Now I'm using 1.  The type hasn't changed at all,
-- so I'm practicing migration w/ a trivial example.
data UserEvent0 = WroteCritique0   DocumentId CritiqueId
                | VotedOnCritique0 DocumentId CritiqueId (Maybe UpDownVote) UTCTime
                | WroteSummary0    DocumentId SummaryId
                | VotedOnSummary0  DocumentId SummaryId (Maybe UpDownVote) UTCTime
                | PostedDocument0  DocumentId
                | FollowedUser0    UserName   UTCTime
                | PinnedDoc0       DocumentId UTCTime
                deriving (Show, Eq, Ord, Generic, Typeable)
deriveSafeCopy 0 'base ''UserEvent0


data User0 = User0 { userName0       :: UserName 
                   , userEmail0      :: Text
                   , userFollowing0  :: Set.Set UserName
                   , userFollowedBy0 :: Set.Set UserName
                   , userHistory0    :: [UserEvent]
                   , userPinboard0   :: Set.Set DocumentId
                   , userTags0       :: Set.Set TagPath
                   , userJoinTime0   :: UTCTime
                   } deriving (Show, Eq, Ord, Generic,Typeable)
deriveSafeCopy 0 'base ''User0

instance Migrate User where
  type MigrateFrom User = User0
  migrate (User0 n e f fb h p t jt) = User n e f fb h p t jt

data UserEvent1 = WroteCritique1   DocumentId CritiqueId
               | VotedOnCritique1 DocumentId CritiqueId (Maybe UpDownVote) UTCTime
               | WroteSummary1    DocumentId SummaryId
               | VotedOnSummary1  DocumentId SummaryId (Maybe UpDownVote) UTCTime
               | PostedDocument1  DocumentId
               | FollowedUser1    UserName   UTCTime
               | PinnedDoc1       DocumentId UTCTime
               deriving (Show, Eq, Ord, Generic, Typeable)
deriveSafeCopy 1 'extension ''UserEvent1

-- Is there a less boilerplate way to do this?
instance Migrate UserEvent1 where
  type MigrateFrom UserEvent1         = UserEvent0
  migrate (WroteCritique0 d c)       = WroteCritique1 d c
  migrate (VotedOnCritique0 d c v t) = VotedOnCritique1 d c v t
  migrate (WroteSummary0 d c)        = WroteSummary1 d c
  migrate (VotedOnSummary0 d c v t)  = VotedOnSummary1 d c v t
  migrate (PostedDocument0 d)        = PostedDocument1 d 
  migrate (FollowedUser0 u t)        = FollowedUser1 u t
  migrate (PinnedDoc0 d t)           = PinnedDoc1 d t

instance Migrate UserEvent where
  type MigrateFrom UserEvent = UserEvent1
  migrate (WroteCritique1 d c) =  WroteOComment d c
  migrate (VotedOnCritique1 d c u t) = VotedOnOComment d c u t
  migrate (WroteSummary1 d c)   = WroteOComment d c
  migrate (VotedOnSummary1 d c u t) = VotedOnOComment d c u t
  migrate (PostedDocument1 d) = PostedDocument d
  migrate (FollowedUser1 un t) = FollowedUser un t
  migrate (PinnedDoc1 d t) = PinnedDoc d t
  