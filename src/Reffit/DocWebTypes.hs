module Reffit.DocWebTypes where

import Reffit.Types

import Data.Text

type Node = Document

data Edge = Edge { edgeClass    :: Text
                 , edgeStrength :: Double
                 , edgeComments :: [Text]
                 , edgeVotes    :: [UserUpDownVote]
                 }

data DocWeb = DocWeb { docWebGraphList :: [(Node,Node,Edge)]
                     }