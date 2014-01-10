{-# LANGUAGE OverloadedStrings #-}

module Reffit.Sort where

import Reffit.Types
import Reffit.Document
import Reffit.FieldTag
import Reffit.Scores
import Reffit.Search

import Text.Printf
import Data.List
import Data.Time
import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.String

data SortBy = New | Hot | Popular | Controversial
            deriving (Show, Eq)


-- |Sort doc list by a function - bool to False for ascending order
--  Highest-scoring paper will be at head of list
sortDocs :: (Document -> Int) -> Bool -> [Document]  -> [Document]
sortDocs scoreF True  = sortBy (\a b -> scoreF b `compare` scoreF a)
sortDocs scoreF False = sortBy (\a b -> scoreF a `compare` scoreF b)


-- TODO pagination.  add Int and Int to each constructor for startInd and #of
data PresentationStrategy = FiltSort SortBy [TagPath]
                          | SearchBy T.Text
                          deriving (Show, Eq)


presentationSort :: UTCTime -> Map.Map DocumentId Document
                    -> PresentationStrategy
                    -> [Document]
presentationSort _ docMap (SearchBy searchTerm) =
  searchDocs 10 docMap searchTerm
presentationSort tNow docMap (FiltSort s [])  =
  (sortDocs (sortF tNow s) True) . Map.elems $ docMap
presentationSort tNow docMap (FiltSort s fts) =
  (sortDocs (sortF tNow s) True)
  . filter (\d -> any (\dtag -> any (tagIncludes dtag) fts) (docFieldTags d))
  . Map.elems $ docMap

sortF :: UTCTime -> SortBy -> (Document -> Int)
sortF t s = case s of
  New -> floor . (\d -> diffUTCTime (docPostTime d) t0)
  Hot -> hotnessScore t
  Popular -> qualityScore
  Controversial -> controversyScore

t0 :: UTCTime
t0 = UTCTime (ModifiedJulianDay 0) 0


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

