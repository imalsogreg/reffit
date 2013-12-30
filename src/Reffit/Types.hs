{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Reffit.Types where

import           Data.Text
import qualified Data.Map as Map
import           GHC.Int
import           GHC.Generics
import           Data.Typeable
import           Reffit.FieldTag
import           Data.Time.Clock
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Set as Set


data DocClass = DocClass { docClassName :: Text
                         } deriving (Show, Generic, Typeable, Eq)
deriveSafeCopy 0 'base ''DocClass

type UserName          = Text
type CritiqueId        = Int32
type SummaryId         = Int32 
type DocumentId        = Int32
type OverviewCommentId = Int32
 
data UpDownVote = DownVote | UpVote
                deriving (Show, Eq, Ord, Generic, Typeable)
deriveSafeCopy 0 'base ''UpDownVote

