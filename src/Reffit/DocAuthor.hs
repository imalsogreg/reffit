{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Reffit.DocAuthor where

------------------------------------------------------------------------------
import           Control.Applicative
import qualified Data.Aeson as A
import           Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Text    as T
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           GHC.Generics
------------------------------------------------------------------------------
import Reffit.User


------------------------------------------------------------------------------
data DocAuthor = DocAuthor {
    daReffitID   :: Maybe UserID
  , daDocID      :: Int
  , daNameString :: T.Text
  } deriving (Eq, Ord, Generic, Show)

instance A.ToJSON   DocAuthor where
  toJSON DocAuthor{..} = A.object ["authorid" .= daReffitID
                                  , "documentid" .= daDocID
                                  , "namestring" .= daNameString]
instance A.FromJSON DocAuthor where
  parseJSON (A.Object v) = DocAuthor
                           <$> v .:? "authorid"
                           <*> v .:  "documentid"
                           <*> v .:  "namestring"
