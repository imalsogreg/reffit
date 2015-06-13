{-# LANGUAGE DeriveGeneric #-}

module Reffit.DocAuthor where

------------------------------------------------------------------------------
import           Control.Applicative
import qualified Data.Aeson as A
import qualified Data.Text    as T
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           GHC.Generics
------------------------------------------------------------------------------
import Reffit.User


-- TODO: Will I ever need these kinds of definitions?
------------------------------------------------------------------------------
data DocAuthor = DocAuthor {
    daReffitID   :: Maybe UserID
  , daDocID      :: Int
  , daNameString :: T.Text
  } deriving (Eq, Ord, Generic, Show)

instance A.ToJSON   DocAuthor where
instance A.FromJSON DocAuthor where

instance FromRow DocAuthor where
  fromRow = DocAuthor <$> field <*> field <*> field
