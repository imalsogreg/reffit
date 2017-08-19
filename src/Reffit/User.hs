{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Reffit.User where

import           Data.Aeson
import           Data.SafeCopy   (Migrate, MigrateFrom, base, deriveSafeCopy,
                                  extension, migrate)
import           Data.Serialize
import qualified Data.Set        as Set
import           Data.Text
import           Data.Time
import           Data.Typeable
import           GHC.Generics

import           Reffit.FieldTag
import           Reffit.Types

data UserEvent = WroteOComment   DocumentId OverviewCommentId
               | VotedOnOComment DocumentId OverviewCommentId (Maybe UpDownVote) UTCTime
               | PostedDocument  DocumentId
               | FollowedUser    UserName   UTCTime
               | PinnedDoc       DocumentId UTCTime
               deriving (Show, Read, Eq, Ord, Generic, Typeable)
deriveSafeCopy 1 'extension ''UserEvent

instance Serialize UserEvent where
instance ToJSON UserEvent
instance FromJSON UserEvent

data User = User { userName       :: UserName
                 , userEmail      :: Text
                 , userFollowing  :: Set.Set UserName
                 , userFollowedBy :: Set.Set UserName
                 , userHistory    :: [UserEvent]
                 , userPinboard   :: Set.Set DocumentId
                 , userTags       :: Set.Set TagPath
                 , userJoinTime   :: UTCTime
                 } deriving (Show, Read, Eq, Ord, Generic,Typeable)
deriveSafeCopy 0 'base ''User

instance Serialize User where
instance ToJSON User
instance FromJSON User

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
  type MigrateFrom UserEvent        = UserEvent0
  migrate (WroteCritique0 d c)       = WroteOComment d c
  migrate (VotedOnCritique0 d c u t) = VotedOnOComment d c u t
  migrate (WroteSummary0 d c)        = WroteOComment d c
  migrate (VotedOnSummary0 d c u t)  = VotedOnOComment d c u t
  migrate (PostedDocument0 d)        = PostedDocument d
  migrate (FollowedUser0 un t)       = FollowedUser un t
  migrate (PinnedDoc0 d t)           = PinnedDoc d t

{-
data User0 = User0 { userName0       :: UserName
                   , userEmail0      :: Text
                   , userFollowing0  :: Set.Set UserName
                   , userFollowedBy0 :: Set.Set UserName
                   , userHistory0    :: [UserEvent0]
                   , userPinboard0   :: Set.Set DocumentId
                   , userTags0       :: Set.Set TagPath
                   , userJoinTime0   :: UTCTime
                   } deriving (Show, Eq, Ord, Generic,Typeable)
deriveSafeCopy 0 'base ''User0

instance Migrate User where
  type MigrateFrom User = User0
<<<<<<< HEAD
  migrate (User0 n e f fb h p t jt) = User n e f fb (Prelude.map migrate h) p t jt
=======
  migrate (User0 n e f fb h p t jt) = User n e f fb h p t jt
>>>>>>> 7fc6b9f38b1bf312e6cd104a1609ec53a6ce8248
-}
