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

data UserEvent = WroteCritique   DocumentId CritiqueId
               | VotedOnCritique DocumentId CritiqueId (Maybe UpDownVote) UTCTime
               | WroteSummary    DocumentId SummaryId
               | VotedOnSummary  DocumentId SummaryId (Maybe UpDownVote) UTCTime
               | PostedDocument  DocumentId
               | FollowedUser    UserName   UTCTime
               | PinnedDoc       DocumentId UTCTime
               deriving (Show, Eq, Ord, Generic, Typeable)
deriveSafeCopy 1 'extension ''UserEvent

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

-- Is there a less boilerplate way to do this?
instance Migrate UserEvent where
  type MigrateFrom UserEvent         = UserEvent0
  migrate (WroteCritique0 d c)       = WroteCritique d c
  migrate (VotedOnCritique0 d c v t) = VotedOnCritique d c v t
  migrate (WroteSummary0 d c)        = WroteSummary d c
  migrate (VotedOnSummary0 d c v t)  = VotedOnSummary d c v t
  migrate (PostedDocument0 d)        = PostedDocument d 
  migrate (FollowedUser0 u t)        = FollowedUser u t
  migrate (PinnedDoc0 d t)           = PinnedDoc d t
  
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