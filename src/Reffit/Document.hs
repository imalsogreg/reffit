{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}

module Reffit.Document where

import           Reffit.Types
import           Reffit.FieldTag
import           Reffit.OverviewComment

import           Control.Applicative
import qualified Data.Text as T
import           Data.Serialize
import           Data.Text
import           Data.Time
import           GHC.Generics
import           Data.Typeable
import           Data.SafeCopy
import qualified Data.Map as Map
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I

-- TODO: Is DocumentHints only used by CrossRef?  If so, it should move there.
data DocumentHints = DocumentHints { titleHint   :: Text
                                   , authorsHint :: [Text]
                                   , linkHint    :: Text
                                   , yearHint    :: Maybe Int
                                   }

                     
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
deriveSafeCopy 0 'base ''Document

instance Serialize Document where

-- I can't write docSummarySplices here, because a view of the document summary
-- depends on scores for the document.  So trying to render it brings in a
-- dependency on Reffit.Scores, but Reffit.Scores depends on Reffit.Document.
-- So PaperRoll is now Reffit.PaperRoll, a module for rendering lists of papers
-- (and, if other modules need, for rendering a single paper-summary block)