{-# LANGUAGE OverloadedStrings #-}

module Reffit.Search where

import Reffit.Types
import Reffit.Document

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List (sortBy, (\\))
import Data.Text.ICU.Normalize
import qualified Data.Char as C

-- TODO: I expect it will be extremely slow to search through
-- the documents like this.  When the type of a Doc settles,
-- can think about the type of an index for the docs.

tokenize :: T.Text -> [T.Text]
tokenize query = T.words query \\ ["A","a","The","the","Is","is","An","an"]

norm :: T.Text -> T.Text
norm t = T.toCaseFold . T.filter C.isAlpha . normalize NFD $ t

dSearchText :: Document -> T.Text
dSearchText doc = T.unwords . concat $ [[docTitle doc], docAuthors doc]

dScore :: T.Text -> Document -> Int
dScore query doc = let qTokens  = tokenize query
                       dText    = dSearchText doc
                       normText = norm dText
                 in
  (length . filter (\t -> T.isInfixOf t dText) $ qTokens) +
  if T.isInfixOf query normText then 10 else 0

searchDocs :: Int -> Map.Map DocumentId Document -> T.Text -> [Document]
searchDocs n docs queryText = let nQueryText = norm queryText in
  take n . 
  sortBy (\a b -> dScore queryText a `Prelude.compare` dScore queryText b) . filter ((>0) . dScore nQueryText) $
  Map.elems docs  