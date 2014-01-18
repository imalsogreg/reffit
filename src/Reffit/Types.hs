{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Reffit.Types where

import           Control.Applicative
import           Data.Text
import qualified Data.Map as Map
import           GHC.Int
import           GHC.Generics
import           Data.Typeable
import           Data.Time.Clock
import           Data.Serialize
import           Data.Time
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Set as Set


-- Orphan instances for types used all over
instance Serialize UTCTime where
  put (UTCTime day dayTime) = do
    put (toModifiedJulianDay day)
    put (toRational dayTime)
  get = UTCTime <$> (ModifiedJulianDay <$> get) <*> (fromRational <$> get)

instance Serialize Text where
  put = put . unpack
  get = pack <$> get

data DocClass = DocClass { docClassName :: Text
                         } deriving (Show, Generic, Typeable, Eq)
deriveSafeCopy 0 'base ''DocClass


type UserName          = Text
type CritiqueId        = Int32
type SummaryId         = Int32
type DocumentId        = Int32
type OverviewCommentId = Int32
type DiscussionPointId = Int32

instance Serialize DocClass where

data UpDownVote = DownVote | UpVote
                deriving (Show, Eq, Ord, Generic, Typeable)
deriveSafeCopy 0 'base ''UpDownVote

instance Serialize UpDownVote where
