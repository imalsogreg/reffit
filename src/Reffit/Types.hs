{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Reffit.Types where

import           Control.Applicative
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.Map as M
import           Data.Text
import           GHC.Int
import           GHC.Generics
import           Data.Typeable
import           Data.Time.Clock
import           Data.Serialize
import           Data.Time
import Data.SafeCopy (base, deriveSafeCopy)


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
                         } deriving (Show, Read, Generic, Typeable, Eq)
deriveSafeCopy 0 'base ''DocClass

instance ToJSON DocClass
instance FromJSON DocClass

type UserName          = Text
type CritiqueId        = Int32
type SummaryId         = Int32
type DocumentId        = Int32
type OverviewCommentId = Int32
type DiscussionPointId = Int32

instance Serialize DocClass where

data UpDownVote = DownVote | UpVote
                deriving (Show, Read, Eq, Ord, Generic, Typeable)
deriveSafeCopy 0 'base ''UpDownVote

instance Serialize UpDownVote where


instance ToJSON UpDownVote
instance FromJSON UpDownVote


-- instance ToJSON v => ToJSON (M.Map Int32 v) where
--     toJSON = toJSON . M.mapKeys show

-- instance FromJSON v => FromJSON (M.Map Int32 v) where
--     parseJSON o = M.mapKeys read <$> parseJSON o
