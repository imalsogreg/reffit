{-# LANGUAGE OverloadedStrings #-}

module HandleIndex(
  handleIndex
  )
where

import Reffit.Types
import PaperRoll

import Snap.Snaplet(Handler)
import Snap.Snaplet.Heist
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T

handleIndex :: Handler App App ()
handleIndex = renderWithSplices "_index" allPaperRollSplices
