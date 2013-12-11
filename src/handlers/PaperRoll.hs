{-# LANGUAGE OverloadedStrings #-}

module PaperRoll (
  handlePaperRoll,
  allPaperRollSplices
  )
       where

import Reffit.Types
import Reffit.AcidTypes


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
  "paper_roll_papers" ## (renderPaperRollPapers docs)

renderPaperRollPapers :: [Document] -> SnapletISplice App
renderPaperRollPapers = I.mapSplices $ I.runChildrenWith . splicesFromDocument 

splicesFromDocument :: Monad n => Document -> Splices (I.Splice n)
splicesFromDocument t = do
  "paper_title"         ## I.textSplice (docTitle t)
  "paper_external_link" ## I.textSplice (docLink t)
  "impact_score"        ## I.textSplice (T.pack $ show (1 ::Int)) --TODO calculate scores
  "rigor_score"         ## I.textSplice (T.pack $ show (2 ::Int))
  "polish_score"        ## I.textSplice (T.pack $ show (3 ::Int))