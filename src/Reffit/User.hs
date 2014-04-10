{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}

module Reffit.User where

import           Reffit.Types
import           Reffit.FieldTag

import           Data.Serialize
import qualified Data.Set as Set
import           Data.Text
import           Data.Time
import           GHC.Generics
import           Data.Typeable
import qualified Data.Aeson as A
import           Data.SafeCopy
import           Control.Lens

data UserEvent = WroteOComment   DocumentId OverviewCommentId
               | VotedOnOComment DocumentId OverviewCommentId (Maybe UpDownVote) UTCTime
               | PostedDocument  DocumentId
               | FollowedUser    UserName   UTCTime
               | PinnedDoc       DocumentId UTCTime
               deriving (Show, Eq, Ord, Generic, Typeable)
deriveSafeCopy 1 'extension ''UserEvent

instance A.ToJSON UserEvent where -- Experiment for later

instance Serialize UserEvent where

data UserEmail = UserEmail { userEmailAddy     :: Text  -- | Experiment for later
                           , userEmailCode     :: Text  -- |
                           , userEmailVerified :: Bool  -- |
                           }
                 deriving (Show, Eq, Ord, Generic, Typeable)
instance Serialize UserEmail
deriveSafeCopy 0 'base ''UserEmail

$(makeLenses ''UserEmail)

data UserFollower = UserFollower { userBeingFollowed :: UserName -- | Experiment,later
                                 , userFollower      :: UserName -- | 
                                 , userFollowerTime  :: UTCTime  -- |
                                 }
                    deriving (Show, Eq, Ord, Generic, Typeable)
instance Serialize UserFollower
deriveSafeCopy 0 'base ''UserFollower

data User0 = User0 { _userName0       :: UserName
                   , _userEmail0      :: Text
                   , _userFollowing0  :: Set.Set UserName
                   , _userFollowedBy0 :: Set.Set UserName
                   , _userPinboard0   :: Set.Set DocumentId
                   , _userHistory0    :: [UserEvent]
                   , _userTags0       :: Set.Set TagPath
                   , _userJoinTime0   :: UTCTime
                   } deriving (Show, Eq, Ord, Generic,Typeable)
deriveSafeCopy 0 'base ''User0

instance Serialize User where

instance Migrate User where
  type MigrateFrom User = User0
  migrate (User0 n e f fb p h t jt) = User n "" e f fb p h t "" "" jt

data User = User { _userName            :: UserName
                 , _userRealName        :: Text
                 , _userEmail           :: Text
                 , _userFollowing       :: Set.Set UserName
                 , _userFollowedBy      :: Set.Set UserName
                 , _userPinboard        :: Set.Set DocumentId
                 , _userHistory         :: [UserEvent]
                 , _userTags            :: Set.Set TagPath
                 , _userWebsite         :: Text
                 , _userDescriptionText :: Text
                 , _userJoinTime        :: UTCTime
                 } deriving (Show, Eq, Ord, Generic, Typeable)
deriveSafeCopy 1 'extension ''User
$(makeLenses ''User)
  
  {-
instance PS.FromRow User where
  PS.fromRow = User <$> field <*> field
               <$> mapM_ PS.toRow (u^.userEmails)
               <$> 
-}
  
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
  migrate (WroteCritique0 d c)       = WroteOComment d c
  migrate (VotedOnCritique0 d c u t) = VotedOnOComment d c u t
  migrate (WroteSummary0 d c)        = WroteOComment d c
  migrate (VotedOnSummary0 d c u t)  = VotedOnOComment d c u t
  migrate (PostedDocument0 d)        = PostedDocument d
  migrate (FollowedUser0 un t)       = FollowedUser un t
  migrate (PinnedDoc0 d t)           = PinnedDoc d t
