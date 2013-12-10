{-# LANGUAGE OverloadedStrings #-}

module HandleViewPaper (handleViewPaper) where

import Reffit.Types
import Reffit.AcidTypes

import Safe
import Snap.Core (getParam)
import Snap.Types (writeText)
import Snap.Snaplet (Handler)
import Snap.Snaplet.AcidState (query)
import Snap.Snaplet.Heist
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
 
handleViewPaper :: Handler App App ()
handleViewPaper = do
  pId' <- getParam "paperid"
  case readMay pId' of
    Nothing -> writeText "Need paperid parameter"
    Just pId ->  do
      docs <- query QueryAllDocs
      case ((==pId) . docId) `filter` docs of
        []    -> writeText "Document wasn't found in the database."
        [doc] -> renderWithSplices "_article_view" (allArticleViewSplices doc)        
        
summarySummary :: Document -> T.Text
summarySummary doc = T.unwords [T.pack (show nVotes)
                                  ,"votes /"
                                  , T.pack (show  nSummaries)
                                  , "summaries"]
  where nSummaries = length . docSummaries $ doc
        nVotes     = sum . map length . docSummaries $ doc
          
-- TODO: Need weighting parameter: democratic? Clout-based?
praiseSummary :: Document -> T.Text
praiseSummary doc = T.concat [concensusPtc
                             ,"% consensus on "
                             , pack (show nPraise)
                             , " points"]
  where nVote ud cs = length . filter (==ud) . attrValue $ map critiqueAttrVote cs
        nUp = nVote UpVote 
                     
allArticleViewSplices :: Document -> Splices (SnapletISplice App)
allArticleViewSplices doc = do
  "articleSummarySummary"   ## I.textSplice (summarySummary doc)
  "articlePraiseSummary"    ## I.textSplice (praiseSummary  doc)
  "articleCriticismSummary" ## I.textSplice (criticismSummary doc)
  "articleSummaries"        ## (renderSummaries  doc)
  "articlePraise"           ## (renderPraise     doc)
  "articleCriticisms"       ## (renderCriticisms doc)