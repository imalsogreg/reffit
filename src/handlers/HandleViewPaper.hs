{-# LANGUAGE OverloadedStrings #-}

module HandleViewPaper (handleViewPaper) where

import Reffit.Types
import Reffit.AcidTypes

import Safe
import Control.Applicative ((<$>),(<*>),pure)
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
import GHC.Int
import Control.Lens
  
handleViewPaper :: Handler App App ()
handleViewPaper = do
  pId' <- getParam "paperid"
  case readMay . T.unpack . decodeUtf8 <$> pId' of
    Nothing -> writeText "Need paperid parameter"
    Just (Just pId) ->  do  -- just just again!
      let a = pId :: Int32
      docs <- query QueryAllDocs
      case ((==pId) . docId) `filter` docs of
        []    -> writeText $ T.concat ["You entered: "
                                       , T.pack (show pId) 
                                       ," Document wasn't found in the database.  We have these ids:"
                                       , T.concat (map (T.pack . show . docId) docs) ]
        [doc] -> renderWithSplices "_article_view" (allArticleViewSplices doc)        

-- |Count positive and negative votes for a summary
summaryUpsDowns :: Summary -> (Int,Int)
summaryUpsDowns s = over both length $
  List.partition (==UpVote) . summaryVotes $ s

critiqueUpsDowns :: Critique -> (Int,Int)
critiqueUpsDowns c = over both length $ 
  List.partition (==UpVote) . critiqueReactions $ c

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
        nVotes     = sum . map length . map summaryVotes . Map.elems . docSummaries $ doc :: Int

critiqueSummary :: Document -> UpDownVote -> T.Text
critiqueSummary doc critType = 
  T.concat [T.pack (show concensusPct),"% consensus on "
           , T.pack (show . length $ critiques), " points"] 
  where 
    critiques = filter ((==critType).critiqueVal) . Map.elems . docCritiques $ doc :: [Critique]
    (nUps,nDowns) = List.partition ((==critType).critiqueVal) critiques & over both length
    concensusPct = floor (fI nUps/ fI (nUps+nDowns) * 100)

fI :: (Integral a, Real b) => a -> b
fI = fromIntegral

a :: Document -> Splices (SnapletISplice App)
a d = (allProseSplices "test" . summariesToProseData . docSummaries $ d)

--b :: Document -> SnapletISplice App
--b d = "articleSummaries" ## a d

c :: (Monad m0, Monad n0) => Document -> HeistT m0 n0 Template 
c d = I.textSplice (summarySummary d)

allArticleViewSplices :: Document -> Splices (SnapletISplice App)
--allArticleViewSplices :: Document -> Splices (I.Splice n)
allArticleViewSplices doc = do
  "articleSummarySummary"   ## I.textSplice (summarySummary doc) :: Splices (SnapletISplice App)
  "articlePraiseSummary"    ## I.textSplice (critiqueSummary  doc UpVote) :: Splices (SnapletISplice App)
  "articleCriticismSummary" ## I.textSplice (critiqueSummary doc DownVote) :: Splices (SnapletISplice App)
  (allProseSplices "articleSummaries" . summariesToProseData . docSummaries $ doc)
  (allProseSplices "articlePraise" . critiquesToProseData UpVote . docCritiques $ doc )
  (allProseSplices "articleCriticisms" . critiquesToProseData DownVote . docCritiques $ doc )
  {-
  "articleSummaries"        ## (allProseSplices . summariesToProseData . docSummaries $ doc )
  "articlePraise"           ## (allProseSplices . critiquesToProseData UpVote . docCritiques $ doc)
  "articleCriticisms"       ## (allProseSplices . critiquesToProseData DownVote . docCritiques $ doc) 
 -}
summariesToProseData :: Map.Map SummaryId Summary -> [(T.Text,Int,Int)]
summariesToProseData ss =
  Map.elems $ Map.map
  (\s -> let (u,d) = summaryUpsDowns s in (summaryProse s, u, d)) ss

critiquesToProseData :: UpDownVote -> Map.Map CritiqueId Critique -> [(T.Text,Int,Int)]
critiquesToProseData targetV cs =
  let targets = filter ((==targetV).critiqueVal) (Map.elems cs) in  
  map (\c -> let (u,d) = critiqueUpsDowns c in (critiqueProse c, u, d)) targets 

compareProseData :: (T.Text,Int,Int) -> (T.Text,Int,Int) -> Ordering
compareProseData (_,aUp,aDown) (_,bUp,bDown) = (aUp - bUp) `compare` (bUp - bDown)

-- analogous to allPaperRollSplices
allProseSplices :: T.Text -> [(T.Text, Int, Int)] -> Splices (SnapletISplice App)
allProseSplices tag ps = tag ## (renderProses ps')
  where ps' = List.sortBy compareProseData ps
 
-- analogous to renderPaperRollPapers
renderProses :: [(T.Text,Int,Int)] -> SnapletISplice App
renderProses = I.mapSplices $ I.runChildrenWith . splicesFromProse 

-- this is analogous to splicesFromDoc
splicesFromProse :: Monad n => (T.Text,Int,Int) -> Splices (I.Splice n) 
splicesFromProse (t,u,d) =  do
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