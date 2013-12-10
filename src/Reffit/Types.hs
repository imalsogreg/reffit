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
import Data.SafeCopy (base, deriveSafeCopy)


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

data UserEvent = WroteCritique   CritiqueId
               | VotedOnCritique CritiqueId (Maybe UpDownVote)
               | WroteSummary    SummaryId
               | VotedOnSummary  SummaryId (Maybe UpDownVote)
               | PostedDocument  DocumentId
               deriving (Show, Eq, Ord, Generic, Typeable)
deriveSafeCopy scv 'base ''UserEvent

data User = User { userName       :: UserName
                 , userFollowing  :: [UserName]
                 , userFollowedBy :: [UserName]
                 , userHistory    :: [UserEvent]
                 } deriving (Show, Eq, Ord, Generic,Typeable)
deriveSafeCopy scv 'base ''User

data Summary = Summary { summaryPoster :: Maybe UserName
                       , summaryProse :: Text
                       , summaryVotes :: [UpDownVote]
                       } deriving (Show, Generic)
deriveSafeCopy scv 'base ''Summary

data QualityDim = Novel | Solid | Cool
                deriving (Show, Generic, Typeable)
deriveSafeCopy scv 'base ''QualityDim

data Critique = Critique { critiquePoster    :: Maybe UserName
                         , critiqueDim       :: QualityDim
                         , critiqueVal       :: UpDownVote
                         , critiqueReactions :: [UpDownVote]
                         } deriving (Show, Generic)
deriveSafeCopy scv 'base ''Critique

data FieldTag = FieldTag { fieldTagText :: Text
                         } deriving (Show, Generic, Typeable)
deriveSafeCopy scv 'base ''FieldTag
                  
data Document = Document { docUploader  :: Maybe UserName
                         , docId        :: DocumentId
                         , docTitle     :: Text
                         , docAuthors   :: [Text]
                         , docLink      :: Text
                         , docClasses   :: DocClass
                         , docFieldTags :: [FieldTag]
                         , docSummaries :: [Summary]
                         , docCritiques :: Map.Map CritiqueId Critique
                         } deriving (Show, Generic, Typeable)
deriveSafeCopy scv 'base ''Document

testPapers :: [Document]
testPapers = [Document Nothing 0 "The Earth is Round (p < .05)" []
              "https://www.ics.uci.edu/~sternh/courses/210/cohen94_pval.pdf" (DocClass "Paper") [] [] Map.empty
             ,Document Nothing 1 "A cool paper about the hippocampus" [] "http://github.com" (DocClass "Paper") [] [] Map.empty
             ] 