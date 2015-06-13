{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reffit.HashTag where

import qualified Data.Aeson as A
import qualified Data.Text as T
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField

newtype HashTag = HashTag {unHashTag :: T.Text}
                deriving (Eq, Ord, Show, A.FromJSON, A.ToJSON, FromField, ToField)

