{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}

module Reffit.Document where

import           Reffit.Types
import           Reffit.FieldTag
import           Reffit.OverviewComment

import           Data.Text
import           Data.Time
import           GHC.Generics
import           Data.Typeable
import           Data.SafeCopy
import qualified Data.Map as Map 

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
