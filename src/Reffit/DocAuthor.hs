module Reffit.DocAuthor where

------------------------------------------------------------------------------
import           Control.Applicative
import qualified Data.Text    as T
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
------------------------------------------------------------------------------
import Reffit.User


------------------------------------------------------------------------------
data DocAuthor = DocAuthor {
    daSurname    :: T.Text
  , daGivenName  :: T.Text
  , reffitID     :: Maybe UserID
  } deriving (Eq, Ord, Show)

instance FromRow DocAuthor where
  fromRow = DocAuthor <$> field <*> field <*> field
