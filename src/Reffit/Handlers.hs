-- Wrapper around all the handler modules for easy importing. 
module Reffit.Handlers 
( module Reffit.Handlers.HandleIndex
  ,module Reffit.Handlers.HandleNewOComment
  ,module Reffit.Handlers.HandleNewDocClass
  ,module Reffit.Handlers.HandleNewPaper
  ,module Reffit.Handlers.HandleSummaryVote
  ,module Reffit.Handlers.HandleViewPaper
  ,module Reffit.Handlers.HandleViewUser
  ,module Reffit.Handlers.HandleAuthentication
  ,module Reffit.Handlers.HandleDiscussion
) where

import Reffit.Handlers.HandleIndex
import Reffit.Handlers.HandleNewOComment
import Reffit.Handlers.HandleNewDocClass
import Reffit.Handlers.HandleNewPaper
import Reffit.Handlers.HandleSummaryVote
import Reffit.Handlers.HandleViewPaper
import Reffit.Handlers.HandleViewUser
import Reffit.Handlers.HandleAuthentication
import Reffit.Handlers.HandleDiscussion
