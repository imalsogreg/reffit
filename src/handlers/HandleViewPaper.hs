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

-- |Count positive and negative votes for a summary
summaryUpsDowns :: Summary -> (Int,Int)
summaryUpsDowns s =
  List.partition ((==UpVote).summaryVotes) & over both length

critiqueUpsDowns :: Critique -> (Int,Int)
critiqueUpsDowns c =
  List.partition ((==UpVote).critiqueReactions) & over both length

compareSummaryNetVote :: Summary -> Summary -> Ordering
compareSummaryNetVote a b = netA `compare` netB
  where (upA,downA) = summaryUpsDowns a
        netA = upA - downA
        (upB,downB) = summaryUpsDowns b
        netB = upB - downB

summarySummary :: Document -> T.Text
summarySummary doc = 
  T.unwords [T.pack (show nVotes),"votes /"
            , T.pack (show  nSummaries), "summaries"]
  where nSummaries = Map.size . docSummaries $ doc 
        nVotes     = sum . map length . Map.elems . docSummaries $ doc :: Int

          
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
  "articleSummaries"        ## (allProseResponses . summariesToProseData . docSummaries $ doc )
  "articlePraise"           ## (allProseResponses . critiquesToProseData UpVote . docCritiques $ doc)
  "articleCriticisms"       ## (allProseResponses . critiquesToProseData DownVote . docCritiques $ doc) 

summariesToProseData :: Map.Map SummaryId Summary -> [(T.Text,Int,Int)]
summariesToProseData ss =
  Map.map
  (\s -> let (u,d) = summaryUpsDowns s in (summaryProse s, u, d)) ss

critiquesToProseData :: UpDownVote -> Map.Map CritiqueId Critique -> [(T.Text,Int,Int)]
critiquesToProseData cs targetV =
  let targets = filter ((==targetV).critiqueVal) cs in 
  Map.map (\c -> let (u,d) = critiqueUpsDowns c in (critiqueProse c, u, d)) targets

compareProseData :: (T.Text,Int,Int) -> (T.Text,Int,Int) -> Ordering
compareProseData (_,aUp,aDown) (_,bUp,bDown) = (aUp - bUp) `compare` (bUp - bDown)

allProseResponses :: [(T.Text, Int, Int)] -> Splices (SnapletISplice App)
allProseResponses ps = renderWithSplices "proseResponses" (renderProses ps')
  where ps' = List.sortBy compareProseData ps

renderProses :: [(T.Text,Int,Int)] -> SnapletISplice App
renderProses ss = I.mapSplices $ I.runChildrenWith . spliceFromProse 

spliceFromProse :: Monad n => (T.Text,Int,Int) -> Splices (I.Splice n)
spliceFromProse (t,u,d) =  do
  "upCount"     ## I.textSplice (T.pack . show $ u)
  "downCount"   ## I.textSplice (T.pack . show $ d)
  "summaryText" ## I.textSplice t

{-
  --TODO : Sort by user's preference.
allSummarySplices :: [Summary]  -> Splices (SnapletISplice App)
allSummarySplices ss = renderWithSplices "proseResponses" (renderSummaries ss')
  where ss' = List.sortBy compareSummaryNetVote ss 

renderSummaries :: Map.Map SummaryId Summary -> SnapletISplice App
renderSummaries ss = I.mapSplices $ I.runChildrenWith . splicesFromSummary
                     
splicesFromSummary :: Monad n => Summary -> Splices (I.Splice n)
splicesFromSummary s = do
  let (nUp,nDown) = summaryUpsDowns s
  "upCount"     ## I.textSplice (T.pack . show $ nUp)
  "downCount"   ## I.textSplice (T.pack . show $ nDown)
  "summaryText" ## I.textSplice (summaryProse s)

allCritiques :: [Critique] -> Splices (SnapletISplice App)
allCritiques cs = renderWithSplices "
-}