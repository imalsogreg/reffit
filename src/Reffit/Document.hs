{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}

module Reffit.Document where

import           Reffit.Types
import           Reffit.FieldTag
import           Reffit.OverviewComment

import qualified Data.Text as T
import           Data.Time
import           GHC.Generics
import           Data.Typeable
import           Data.SafeCopy
import qualified Data.Map as Map
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I

-- TODO: Is DocumentHints only used by CrossRef?  If so, it should move there.
data DocumentHints = DocumentHints { titleHint   :: T.Text
                                   , authorsHint :: [T.Text]
                                   , linkHint    :: T.Text
                                   , yearHint    :: Maybe Int
                                   }

                     
data Document = Document { docUploader  :: Maybe UserName
                         , docId        :: DocumentId
                         , docTitle     :: T.Text
                         , docAuthors   :: [T.Text]
                         , docLink      :: T.Text
                         , docClass     :: DocClass
                         , docFieldTags :: [TagPath]
                         , docOComments :: Map.Map OverviewCommentId OverviewComment
                         , docPostTime  :: UTCTime
                         } deriving (Show, Generic, Typeable)
deriveSafeCopy 1 'extension ''Document 

data Document0 = Document0 { docUploader0  :: Maybe UserName
                           , docId0        :: DocumentId
                           , docTitle0     :: T.Text
                           , docAuthors0   :: [T.Text]
                           , docLink0      :: T.Text
                           , docClass0     :: DocClass
                           , docFieldTags0 :: [TagPath]
                           , docSummaries0 :: Map.Map SummaryId  Summary   -- TODO remove this
                           , docCritiques0 :: Map.Map CritiqueId Critique  -- TODO remove this
                           , docPostTime0  :: UTCTime
                           } deriving (Show, Generic, Typeable)
deriveSafeCopy 0 'base ''Document0

instance Migrate Document where
  type MigrateFrom Document = Document0
  migrate (Document0 u i title a l c tags sums crits t) = 
    Document u i title a l c tags oComs t
    where
      -- Possible bug here: we're doing a Map.union.  If a summary shares
      -- and a critique have the same id (as can happen if their text is
      -- exactly the same, then we expect the critique to be discarded.
      -- Fortunately this migration is only happening once, when there is
      -- almost no content on the site!
      oComs = Map.union (Map.map sumToComm sums) (Map.map critToComm crits)
      sumToComm  (Summary sP sPr sVs sT) =
        OverviewComment sP sPr Nothing sVs sT
      critToComm (Critique cPr cp cDim cVal cReac cT) =
        OverviewComment cp cPr (Just (cDim, cVal)) cReac cT