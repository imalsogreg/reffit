{-# LANGUAGE OverloadedStrings #-}

module HandleViewPaper (handleViewPaper) where

import Reffit.Types
import Reffit.AcidTypes

import Safe
import qualified Data.List as List
import qualified Data.Map as Map
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
import Control.Lens
  
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
critiqueSummary :: Document -> UpDownVote -> T.Text
critiqueSummary doc critType = 
  T.concat [T.pack (show concensusPct),"% consensus on "
           , T.pack (show . length $ critiques), " points"] 
  where 
    critiques = filter ((==critType).critiqueVal) $ Map.elems docCritiques doc
    (nUps,nDowns) = List.partition (==critType) critiques & over both length
    concensusPct = floor (nUps/(nUps+nDowns) * 100)

allArticleViewSplices :: Document -> Splices (SnapletISplice App)
allArticleViewSplices doc = do
  "articleSummarySummary"   ## I.textSplice (summarySummary doc)
  "articlePraiseSummary"    ## I.textSplice (critiqueSummary  doc UpVote)
  "articleCriticismSummary" ## I.textSplice (critiqueSummary doc DownVote)
  "articleSummaries"        ## (renderSummaries  doc)
  "articlePraise"           ## (renderPraise     doc)
  "articleCriticisms"       ## (renderCriticisms doc)