{-# LANGUAGE OverloadedStrings #-}

module PaperRoll (
  handlePaperRoll
  )
       where

import Reffit.Types

import Snap.Snaplet (Handler)
import Snap.Snaplet.Heist
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T
 
handlePaperRoll :: Handler App App ()
handlePaperRoll = renderWithSplices "paper_roll" allPaperRollSplices

allPaperRollSplices :: Splices (SnapletISplice App)
allPaperRollSplices = "paper_roll_papers" ## (renderPaperRollPapers testPapers)

renderPaperRollPapers :: [Document] -> SnapletISplice App
renderPaperRollPapers = I.mapSplices $ I.runChildrenWith . splicesFromDocument

splicesFromDocument :: Monad n => Document -> Splices (I.Splice n)
splicesFromDocument t = do
  "paper_title"         ## I.textSplice (docTitle t)
  "paper_external_link" ## I.textSplice (docLink t)
  "impact_score"        ## I.textSplice (T.pack $ show (1 ::Int)) --TODO calculate scores
  "rigor_score"         ## I.textSplice (T.pack $ show (2 ::Int))
  "polish_score"        ## I.textSplice (T.pack $ show (3 ::Int))