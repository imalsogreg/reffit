{-# LANGUAGE DeriveDataTypeable #-}

module Reffit.Types where

import           Data.Text
import qualified Data.Map as Map
import           GHC.Int
import           Data.Typeable

data DocClass = DocClass { docClassName :: Text
                         }

data User = User { userId   :: Int32
                 , userName :: Text
                 }

data UpDownVote = UpVote | DownVote | NeutralVote

data UserUpDownVote = UserUpDownVote { upDownVoteUser :: User
                                     , upDownVoteAnon :: Bool
                                     , upDownVoteVal  :: UpDownVote
                                     }

data Summary = Summary { docSummaryProse :: Text
                       , docSummaryVotes :: [UserUpDownVote]
                       }

data Critique = Critique { critiqueUser     :: User
                         , critiqueAnon     :: Bool
                         , critiqueAttrVote :: AttrVote
                         }

data AttrVote = AttrVote { attrClass :: Text
                         , attrValue :: UserUpDownVote
                         }

data FieldTag = FieldTag { fieldTagText :: Text
                         }

data Document = Document { docId        :: Int32
                         , docTitle     :: Text
                         , docLink      :: Text
                         , docClasses   :: [DocClass]
                         , docFieldTags :: [FieldTag]
                         , docSummaries :: [Summary]
                         , docCritiques :: Map.Map User Critique
                         } deriving (Typeable)

