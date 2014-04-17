{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}

module Reffit.OverviewComment where

import           Reffit.Types
import           Reffit.Discussion

import           Data.Serialize
import           Data.Text
import           Data.Time
import           GHC.Generics
import           Data.Typeable
import           Data.SafeCopy

data OverviewCommentType = Summary' | Praise | Criticism
                         deriving (Eq, Show, Generic, Typeable)
deriveSafeCopy 0 'base ''OverviewCommentType

data QualityDim = Novelty | Rigor | Coolness
                deriving (Eq, Show, Generic, Typeable)
deriveSafeCopy 0 'base ''QualityDim

instance Serialize QualityDim where

data OverviewComment = OverviewComment
                       { ocPoster :: Maybe UserName
                       , ocText   :: Text
                       , ocVote   :: Maybe (QualityDim,UpDownVote)
                       , ocResponse :: [UpDownVote]
                       , ocPostTime :: UTCTime
                       , ocDiscussion :: Discussion
                       } deriving (Eq, Show, Generic)
deriveSafeCopy 1 'extension ''OverviewComment

instance Serialize OverviewComment where

data OverviewComment_v0 = OverviewComment_v0
                       { ocPoster0 :: Maybe UserName
                       , ocText0   :: Text
                       , ocVote0   :: Maybe (QualityDim,UpDownVote)
                       , ocResponse0 :: [UpDownVote]
                       , ocPostTime0 :: UTCTime
                       } deriving (Show, Generic)
deriveSafeCopy 0 'base ''OverviewComment_v0

instance Migrate OverviewComment where
  type MigrateFrom OverviewComment = OverviewComment_v0
  migrate (OverviewComment_v0 ocp oct ocv ocrs ocpt) =
    OverviewComment ocp oct ocv ocrs ocpt emptyDiscussion

-- Depricated.  Summary and Critique types subsumed
-- by OverviewComment
data Summary = Summary { summaryPoster   :: Maybe UserName
                       , summaryProse    :: Text
                       , summaryVotes    :: [UpDownVote]
                       , summaryPostTime :: UTCTime
                       } deriving (Show, Generic)
deriveSafeCopy 0 'base ''Summary

-- TODO: Are these covered by migrate?
summToOComment :: Summary -> OverviewComment_v0
summToOComment (Summary un pr reacs t) =
  OverviewComment_v0 un pr Nothing reacs t

critToOComment :: Critique -> OverviewComment_v0
critToOComment (Critique pr un qDim val reacs t) =
  OverviewComment_v0 un pr (Just (qDim,val)) reacs t

instance Serialize Summary where


data Critique = Critique { critiqueProse     :: Text
                         , critiquePoster    :: Maybe UserName
                         , critiqueDim       :: QualityDim
                         , critiqueVal       :: UpDownVote
                         , critiqueReactions :: [UpDownVote]
                         , critiquePostTime  :: UTCTime
                         } deriving (Show, Generic)
deriveSafeCopy 0 'base ''Critique

instance Serialize Critique where
