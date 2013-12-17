{-# LANGUAGE OverloadedStrings #-}

module PaperRoll (
  handlePaperRoll,
  allPaperRollSplices
  )
       where

import Reffit.Types
import Reffit.AcidTypes
import Reffit.Scores

import qualified Data.List as L
import Snap.Snaplet (Handler)
import Snap.Snaplet.AcidState (query)
import Snap.Snaplet.Heist
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import qualified Data.Map as Map
  
handlePaperRoll :: Handler App App ()
handlePaperRoll = do
  docs <- query QueryAllDocs                
  renderWithSplices "paper_roll" (allPaperRollSplices (Map.elems docs)) 

allPaperRollSplices :: [Document] -> Splices (SnapletISplice App)
allPaperRollSplices docs = do
  "paper_roll_papers" ## (renderPaperRollPapers (take 100 docs))
 
renderPaperRollPapers :: [Document] -> SnapletISplice App
renderPaperRollPapers = I.mapSplices $ I.runChildrenWith . splicesFromDocument 

splicesFromDocument :: Document -> Splices (SnapletISplice App) 
splicesFromDocument doc = do
  let (novScore, rigScore, coolScore) = documentScores doc 
  "idNum"               ## I.textSplice (T.pack . show $ docId doc)
  "paper_title"         ## I.textSplice (docTitle doc)
  "paper_authors"       ## I.textSplice (T.intercalate ", " $ docAuthors doc)  
  "paper_external_link" ## I.textSplice (docLink doc)
  "noveltyScore"        ## I.textSplice (T.pack $ show (novScore))
  "rigorScore"          ## I.textSplice (T.pack $ show (rigScore))
  "coolnessScore"       ## I.textSplice (T.pack $ show (coolScore))
  (allFieldTags $ docFieldTags doc)
 
allFieldTags :: [FieldTag] -> Splices (SnapletISplice App)
allFieldTags tags = "fieldTags" ## renderFieldTags fLabels
    where
      fLabels = map fieldTagText tags

renderFieldTags :: [T.Text] -> SnapletISplice App
renderFieldTags = I.mapSplices $ I.runChildrenWith . splicesFromTag 

splicesFromTag :: Monad n => T.Text -> Splices (I.Splice n)
splicesFromTag t = do
  "fieldTag" ## I.textSplice t
