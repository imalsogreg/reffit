{-# LANGUAGE OverloadedStrings #-}

module HandleIndex(
  handleIndex
  )
where

import Reffit.Types

import Snap.Snaplet(Handler)
import Snap.Snaplet.Heist
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T

handleIndex :: Handler App App ()
handleIndex = renderWithSplices "index" testSplices 

testSplices :: (Monad n) => Splices (I.Splice n)
testSplices = do
  "testString" ## I.textSplice ("HELLO")