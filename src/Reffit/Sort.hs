{-# LANGUAGE OverloadedStrings #-}

module Reffit.Sort where

import Reffit.Types
import Reffit.Scores

import Text.Printf
import Data.List
import Data.Time
import Data.Time.Clock
import qualified Data.Text as T
import Data.String

data SortBy = New | Hot | Popular | Controversial
            deriving (Show, Eq)


-- |Sort doc list by a function - bool to False for ascending order
--  Highest-scoring paper will be at head of list
sortDocs :: (Document -> Int) -> Bool -> [Document]  -> [Document]
sortDocs scoreF True  = sortBy (\a b -> scoreF b `compare` scoreF a)
sortDocs scoreF False = sortBy (\a b -> scoreF a `compare` scoreF b)

readSort :: (IsString a,Eq a) => a -> Maybe SortBy
readSort "New"           = Just New
readSort "Hot"           = Just Hot
readSort "Popular"       = Just Popular
readSort "Controversial" = Just Controversial
readSort _               = Nothing

sayTimeDiff :: UTCTime -> UTCTime -> String
sayTimeDiff a@(UTCTime aDate _) b@(UTCTime bDate _)
  | dYear > 1  = printf "%d years ago" dYear
  | dYear == 1 && dMonth > 11 = printf "1 year ago"
  | dMonth > 1 = printf "%d months ago" dMonth
  | dMonth == 1 && dDay > 27 = printf "1 month ago"
  | dDay > 1    = printf "%d days ago" dDay
  | dDay == 1 && dHours > 23 = printf "1 day ago"
  | dHours > 1  = printf "%d hours ago" dHours
  | dHours == 1 = printf "1 hour ago"
  | dMins > 1   = printf "%d minutes ago" dMins
  | dMins == 1  = printf "1 minute ago"
  | dSecs > 1   = printf "%d seconds ago" dSecs 
  | otherwise   = printf "Just now"
  where dt = realToFrac $ diffUTCTime a b :: Double
        dSecs  = floor $ dt             :: Int
        dMins  = floor $ dt / 60        :: Int
        dHours = floor $ dt / (3600)    :: Int
        (aYear, aMonth, aDay) = toGregorian aDate
        (bYear, bMonth, bDay) = toGregorian bDate
        dYear  = aYear  - bYear
        dMonth = aMonth - bMonth
        dDay   = aDay   - bDay

