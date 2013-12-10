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

handleIndex :: Handler App App ()
handleIndex = do
  docs <- query QueryAllDocs             
  renderWithSplices "_index" (allPaperRollSplices docs)
