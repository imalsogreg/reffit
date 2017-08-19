{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Reffit.Discussion where


import           Control.Lens
import           Data.Aeson
import qualified Data.Foldable  as F
import           Data.SafeCopy
import           Data.Serialize
import qualified Data.Text      as T
import           Data.Time
import qualified Data.Tree      as Tree
import           GHC.Generics

import           Reffit.Types

type Discussion = Tree.Forest DiscussionPoint

emptyDiscussion :: Discussion
emptyDiscussion = []

data DiscussionPoint = DiscussionPoint
                  { _dID       :: DiscussionPointId
                  , _dPoster   :: Maybe UserName
                  , _dText     :: T.Text
                  , _dResponse :: [UpDownVote]
                  , _dContext  :: (DocumentId  -- TODO is the _dContext
                                               -- field helpful? Or is it
                                               -- mixing a 'name' into a
                                               -- a type that should be
                                               -- tracked from its context
                                               -- outside?
                                  , Maybe OverviewCommentId
                                  , Maybe DiscussionPointId)
                  , _dPostTime :: UTCTime
                  } deriving (Eq, Read, Show, Generic)

$(makeLenses ''DiscussionPoint)
deriveSafeCopy 0 'base ''DiscussionPoint
instance ToJSON DiscussionPoint
instance FromJSON DiscussionPoint


instance Serialize DiscussionPoint where

discussionSize :: Discussion -> Int
discussionSize = length . F.toList

insertAt :: DiscussionPoint -> Maybe DiscussionPointId -> Discussion -> Discussion
insertAt dp Nothing         dps = dps ++ [Tree.Node dp []]
insertAt dp (Just parentId) dps = map (\t -> t {Tree.subForest = children' t}) dps
  where
    children' (Tree.Node rootD childDs)
      | _dID rootD == parentId = childDs ++ [Tree.Node dp []]
      | otherwise = insertAt dp (Just parentId) childDs

testDiscussion :: Discussion
testDiscussion =
  [ Tree.Node
    (DiscussionPoint 0 (Just "Greg") "That was a really cool document" [] (0,Nothing,Nothing) t0)
    [ Tree.Node (DiscussionPoint 1 (Just "Gerg") "Here here!" [] (0,Nothing,Nothing) t0) []
    , Tree.Node (DiscussionPoint 2 (Just "Bob") "You would think that, huh?" [] (0,Nothing,Nothing) t0)
      [
        Tree.Node (DiscussionPoint 3 (Just "Greg") "Yeah, what of it?" [] (0,Nothing,Nothing) t0) []
      ]
    ]
  , Tree.Node (DiscussionPoint 4 (Just "LateGuy") "Comin' in late!" [] (0,Nothing,Nothing) t0) []
  ]

t0 :: UTCTime
t0 = UTCTime (ModifiedJulianDay 0) (fromIntegral (0::Int))
