{-# LANGUAGE OverloadedStrings #-}

module HandleIndex(
  handleIndex
  )
where

import Reffit.Types
import PaperRoll
import Reffit.AcidTypes

import Snap.Snaplet(Handler)
import Snap.Snaplet.AcidState (query)
import Snap.Snaplet.Heist
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import qualified Data.Map as Map 

handleIndex :: Handler App App ()
--handleIndex = handlePaperRoll
handleIndex = do
  docs  <- query QueryAllDocs
  us    <- query QueryAllUsers
--  renderWithSplices "_index" (allPaperRollSplices (Map.elems docs))
  renderWithSplices "_index" (allIndexSplices docs us)

allIndexSplices :: Map.Map DocumentId Document -> Map.Map UserName User -> Splices (SnapletISplice App)
allIndexSplices docs us = do
  allPaperRollSplices (Map.elems docs)
  allStatsSplices docs us


allStatsSplices :: Map.Map DocumentId Document -> Map.Map UserName User -> Splices (SnapletISplice App)
allStatsSplices docs us = do
  "nUsers"    ## I.textSplice $ T.pack . show . Map.size $ us
  "nDocs"     ## I.textSplice $ T.pack . show . Map.size $ docs
  "nComments" ## I.textSplice $ T.pack . show $
    sum (map (\d -> (Map.size . docSummaries $ d) + (Map.size . docCritiques $ d)) (Map.elems docs))
  "nVotes"    ## I.textSplice $ T.pack . show $
    sum (map (\d -> (sum $ map (length . summaryVotes) (Map.elems $ docSummaries d)) +
                    (sum $ map (length . critiqueReactions) (Map.elems $ docCritiques d))) (Map.elems docs)) 