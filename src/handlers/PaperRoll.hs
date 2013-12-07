module PaperRoll (
  handlePaperRoll)
  where

import Reffit.Types

handlePaperRoll :: Handler App App ()
handlePaperRoll = rendewWithSplices "/paper_roll" 