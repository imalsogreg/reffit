module Reffit.HashTag where

import qualified Data.Text as T

newtype HashTag = HashTag {unHashTag :: T.Text}
                deriving (Eq, Ord, Show)

