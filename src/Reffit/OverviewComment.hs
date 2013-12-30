{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}

module Reffit.OverviewComment where

import           Reffit.Types
import           Reffit.FieldTag

import           Data.Text
import           Data.Time
import           GHC.Generics
import           Data.Typeable
import           Data.SafeCopy
import qualified Data.Map as Map 


data QualityDim = Novelty | Rigor | Coolness
                deriving (Eq, Show, Generic, Typeable)
deriveSafeCopy 0 'base ''QualityDim

data OverviewCommentType = Summary' | Praise | Criticism
                         deriving (Eq, Show, Generic, Typeable)
deriveSafeCopy 0 'base ''OverviewCommentType

data OverviewComment = OverviewComment { ocPoster :: Maybe UserName
                                       , ocText   :: Text
                                       , ocVote   :: Maybe (QualityDim,UpDownVote)
                                       , ocResponse :: [UpDownVote]
                                       , ocPostTime :: UTCTime
                                         -- (coming soon, threaded Discussions!)
                                         -- ocDiscussions :: Forrest DiscussionComment 
                                       } deriving (Show, Generic)
deriveSafeCopy 0 'base ''OverviewComment


data Summary = Summary { summaryPoster   :: Maybe UserName
                       , summaryProse    :: Text
                       , summaryVotes    :: [UpDownVote]
                       , summaryPostTime :: UTCTime
                       } deriving (Show, Generic)
deriveSafeCopy 0 'base ''Summary

data Critique = Critique { critiqueProse     :: Text
                         , critiquePoster    :: Maybe UserName
                         , critiqueDim       :: QualityDim
                         , critiqueVal       :: UpDownVote
                         , critiqueReactions :: [UpDownVote]
                         , critiquePostTime  :: UTCTime
                         } deriving (Show, Generic)
deriveSafeCopy 0 'base ''Critique

