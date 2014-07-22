module Reffit.DocAuthor where

------------------------------------------------------------------------------
import           Data.Maybe   (listToMaybe)
import qualified Data.Text    as T
import           Snap.Snaplet (Handler)
------------------------------------------------------------------------------
import Reffit.User


------------------------------------------------------------------------------
data DocAuthor = DocAuthor {
    daSurname    :: T.Text
  , daGivenName  :: T.Text
  , reffitID     :: Maybe UserID
  } deriving (Eq, Ord, Show)
