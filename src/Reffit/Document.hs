{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}

module Reffit.Document where

import           Reffit.Types
import           Reffit.FieldTag
import           Reffit.OverviewComment
import           Reffit.Discussion

import qualified Data.Text as T
import           Data.Serialize
import           Data.Time
import           GHC.Generics
import           Data.Typeable
import           Data.SafeCopy
import qualified Data.Map as Map

data Document = Document
                { docUploader   :: Maybe UserName
                , docId         :: DocumentId
                , docTitle      :: T.Text
                , docAuthors    :: [T.Text]
                , docLink       :: T.Text
                , docClass      :: DocClass
                , docFieldTags  :: [TagPath]
                , docOComments  :: Map.Map OverviewCommentId OverviewComment
                , docPostTime   :: UTCTime
                , docDiscussion :: Discussion
                } deriving (Show, Read, Generic, Typeable)
deriveSafeCopy 2 'extension ''Document
instance Serialize Document where

data Document_v1 = Document_v1
                   { docUploader1  :: Maybe UserName
                   , docId1        :: DocumentId
                   , docTitle1     :: T.Text
                   , docAuthors1   :: [T.Text]
                   , docLink1      :: T.Text
                   , docClass1     :: DocClass
                   , docFieldTags1 :: [TagPath]
                   , docOComments1 :: Map.Map OverviewCommentId OverviewComment_v0
                   , docPostTime1  :: UTCTime
                   } deriving (Show, Generic, Typeable)
deriveSafeCopy 1 'extension ''Document_v1


instance Migrate Document where
  type MigrateFrom Document = Document_v1
  migrate (Document_v1 u i title a l c tags coms t) =
    Document u i title a l c tags (Map.map migrate coms) t []

data Document_v0 = Document_v0 { docUploader0  :: Maybe UserName
                               , docId0        :: DocumentId
                               , docTitle0     :: T.Text
                               , docAuthors0   :: [T.Text]
                               , docLink0      :: T.Text
                               , docClass0     :: DocClass
                               , docFieldTags0 :: [TagPath]
                               , docSummaries0 :: Map.Map SummaryId  Summary
                               , docCritiques0 :: Map.Map CritiqueId Critique
                               , docPostTime0  :: UTCTime
                               } deriving (Show, Generic, Typeable)
deriveSafeCopy 0 'base ''Document_v0

instance Migrate Document_v1 where
  type MigrateFrom Document_v1 = Document_v0
  migrate (Document_v0 u i title a l c tags sums crits t) =
    Document_v1 u i title a l c tags oComs t
    where
      -- Possible bug here: we're doing a Map.union.  If a summary shares
      -- and a critique have the same id (as can happen if their text is
      -- exactly the same, then we expect the critique to be discarded.
      -- Fortunately this migration is only happening once, when there is
      -- almost no content on the site!
      oComs :: Map.Map OverviewCommentId OverviewComment_v0
      oComs = Map.union (Map.map sumToComm sums) (Map.map critToComm crits)
      sumToComm :: Summary -> OverviewComment_v0
      sumToComm  (Summary sP sPr sVs sT) =
        OverviewComment_v0 sP sPr Nothing sVs sT
      critToComm (Critique cPr cp cDim cVal cReac cT) =
        OverviewComment_v0 cp cPr (Just (cDim, cVal)) cReac cT

-- I can't write docSummarySplices here, because a view of the document summary
-- depends on scores for the document.  So trying to render it brings in a
-- dependency on Reffit.Scores, but Reffit.Scores depends on Reffit.Document.
-- So PaperRoll is now Reffit.PaperRoll, a module for rendering lists of papers
-- (and, if other modules need, for rendering a single paper-summary block)

