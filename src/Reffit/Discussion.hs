{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}

module Reffit.Discussion where

import Reffit.Types

import qualified Data.Tree as Tree
import qualified Data.Text as T
import Control.Lens
import Data.Serialize
import qualified Data.Foldable as F
import Data.SafeCopy
import GHC.Generics

type Discussion = Tree.Forest DiscussionPoint

emptyDiscussion :: Discussion
emptyDiscussion = []

data DiscussionPoint = DiscussionPoint
                  { _dID       :: DiscussionPointId  
                  , _dPoster   :: Maybe UserName
                  , _dText     :: T.Text
                  , _dResponse :: [UpDownVote]
                  , _dContext  :: (DocumentId, Maybe OverviewCommentId)
                  } deriving (Eq, Show, Generic)

$(makeLenses ''DiscussionPoint)
deriveSafeCopy 0 'base ''DiscussionPoint


instance Serialize DiscussionPoint where

discussionSize :: Discussion -> Int
discussionSize = length . F.toList

insertAt :: DiscussionPoint -> DiscussionPointId -> Discussion -> Discussion
insertAt dp parentId dps = map (\t -> t {Tree.subForest = children' t}) dps
  where
    children' (Tree.Node rootD childDs)
      | _dID rootD == parentId = childDs ++ [Tree.Node dp []]
      | otherwise = insertAt dp parentId childDs
