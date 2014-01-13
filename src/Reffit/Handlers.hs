-- Wrapper around all the handler modules for easy importing. 
module Reffit.Handlers 
( module Reffit.Handlers.HandleIndex
  ,module Reffit.Handlers.HandleNewCritique
  ,module Reffit.Handlers.HandleNewDocClass
  ,module Reffit.Handlers.HandleNewPaper
  ,module Reffit.Handlers.HandleNewSummary
  ,module Reffit.Handlers.HandleSummaryVote
  ,module Reffit.Handlers.HandleViewPaper
  ,module Reffit.Handlers.HandleViewUser
) where

import Reffit.Handlers.HandleIndex
import Reffit.Handlers.HandleNewCritique
import Reffit.Handlers.HandleNewDocClass
import Reffit.Handlers.HandleNewPaper
import Reffit.Handlers.HandleNewSummary
import Reffit.Handlers.HandleSummaryVote
import Reffit.Handlers.HandleViewPaper
import Reffit.Handlers.HandleViewUser
