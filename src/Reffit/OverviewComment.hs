{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}

module Reffit.OverviewComment where

import           Reffit.Types
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

data OverviewComment = OverviewComment { ocPoster :: Maybe UserName
                                       , ocText   :: Text
                                       , ocVote   :: Maybe (QualityDim,UpDownVote)
                                       , ocResponse :: [UpDownVote]
                                       , ocPostTime :: UTCTime
                                         -- (coming soon, threaded Discussions!)
                                         -- ocDiscussions :: Forrest DiscussionComment
                                       } deriving (Show, Generic)
deriveSafeCopy 0 'base ''OverviewComment

instance Serialize OverviewComment where

data Summary = Summary { summaryPoster   :: Maybe UserName
                       , summaryProse    :: Text
                       , summaryVotes    :: [UpDownVote]
                       , summaryPostTime :: UTCTime
                       } deriving (Show, Generic)
deriveSafeCopy 0 'base ''Summary

-- TODO: Are these covered by migrate?
summToOComment :: Summary -> OverviewComment
summToOComment (Summary un pr reacs t) =
  OverviewComment un pr Nothing reacs t

critToOComment :: Critique -> OverviewComment
critToOComment (Critique pr un qDim val reacs t) =
  OverviewComment un pr (Just (qDim,val)) reacs t

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
