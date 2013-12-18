{-# LANGUAGE OverloadedStrings #-}

module PaperRoll (
  handlePaperRoll,
  allPaperRollSplices
  )
       where

import Reffit.Types
import Reffit.AcidTypes
import Reffit.Scores
import Reffit.Search
import Reffit.FieldTag

import Control.Applicative
import qualified Data.List as L
import Snap.Snaplet (Handler)
import Snap.Core
import Snap.Snaplet.AcidState (query)
import Snap.Snaplet.Heist
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as Map
  
handlePaperRoll :: Handler App App ()
handlePaperRoll = method GET handleNotGet
  where  
    handleNotGet = 
      do
        docs      <- query QueryAllDocs
        searchFor <- getParams
        let docsToShow = case Map.lookup "q" searchFor of
              Nothing         -> Map.elems docs
              Just searchTerms -> searchDocs 10 docs $ decodeUtf8 (head searchTerms)
        renderWithSplices "paper_roll" (allPaperRollSplices docsToShow)


allPaperRollSplices :: [Document] -> Splices (SnapletISplice App)
allPaperRollSplices docs = do
  "paper_roll_papers" ## (renderPaperRollPapers (take 100 docs))
  
renderPaperRollPapers :: [Document] -> SnapletISplice App
renderPaperRollPapers = I.mapSplices $ I.runChildrenWith . splicesFromDocument 

splicesFromDocument :: Document -> Splices (SnapletISplice App) 
splicesFromDocument doc = do
  let (novScore, rigScore, coolScore) = documentDimScores doc 
  "idNum"               ## I.textSplice (T.pack . show $ docId doc)
  "paper_title"         ## I.textSplice (docTitle doc)
  "paper_authors"       ## I.textSplice (T.intercalate ", " $ docAuthors doc)  
  "paper_external_link" ## I.textSplice (docLink doc)
  "noveltyScore"        ## I.textSplice (T.pack $ show (novScore))
  "rigorScore"          ## I.textSplice (T.pack $ show (rigScore))
  "coolnessScore"       ## I.textSplice (T.pack $ show (coolScore))
  (allFieldTags $ docFieldTags doc)
 
allFieldTags :: [TagPath] -> Splices (SnapletISplice App)
allFieldTags tags = "fieldTags" ## renderFieldTags fLabels
    where
      fLabels = map last tags

renderFieldTags :: [T.Text] -> SnapletISplice App
renderFieldTags = I.mapSplices $ I.runChildrenWith . splicesFromTag 

splicesFromTag :: Monad n => T.Text -> Splices (I.Splice n)
splicesFromTag t = do
  "fieldTag" ## I.textSplice t
