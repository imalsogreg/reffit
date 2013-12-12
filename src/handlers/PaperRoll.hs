{-# LANGUAGE OverloadedStrings #-}

module PaperRoll (
  handlePaperRoll,
  allPaperRollSplices
  )
       where

import Reffit.Types
import Reffit.AcidTypes

import qualified Data.List as L
import Snap.Snaplet (Handler)
import Snap.Snaplet.AcidState (query)
import Snap.Snaplet.Heist
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T
 
handlePaperRoll :: Handler App App ()
handlePaperRoll = do
  docs <- query QueryAllDocs                
  renderWithSplices "paper_roll" (allPaperRollSplices docs)

allPaperRollSplices :: [Document] -> Splices (SnapletISplice App)
allPaperRollSplices docs = do
  "paper_roll_papers" ## (renderPaperRollPapers (take 100 docs))
 
renderPaperRollPapers :: [Document] -> SnapletISplice App
renderPaperRollPapers = I.mapSplices $ I.runChildrenWith . splicesFromDocument 

--splicesFromDocument :: Monad n => Document -> Splices (I.Splice n)
splicesFromDocument :: Document -> Splices (SnapletISplice App) 
splicesFromDocument t = do
  "idNum"               ## I.textSplice (T.pack . show $ docId t)
  "paper_title"         ## I.textSplice (docTitle t)
  "paper_authors"       ## I.textSplice (T.intercalate ", " $ docAuthors t)  
  "paper_external_link" ## I.textSplice (docLink t)
  "noveltyScore"        ## I.textSplice (T.pack $ show (1 ::Int)) --TODO calculate scores
  "rigorScore"          ## I.textSplice (T.pack $ show (2 ::Int))
  "coolnessScore"       ## I.textSplice (T.pack $ show (3 ::Int))
  (allFieldTags $ docFieldTags t)
 
allFieldTags :: [FieldTag] -> Splices (SnapletISplice App)
allFieldTags tags = "fieldTags" ## renderFieldTags fLabels
    where
      fLabels = map fieldTagText tags

renderFieldTags :: [T.Text] -> SnapletISplice App
renderFieldTags = I.mapSplices $ I.runChildrenWith . splicesFromTag 

splicesFromTag :: Monad n => T.Text -> Splices (I.Splice n)
splicesFromTag t = do
  "fieldTag" ## I.textSplice t
