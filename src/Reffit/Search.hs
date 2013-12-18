module Reffit.Search where

import Reffit.Types
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List (sortBy)
import Data.Maybe (catMaybes)

-- TODO: I expect it will be extremely slow to search through
-- the documents like this.  When the type of a Doc settles,
-- can think about the type of an index for the docs.

stripToWord :: T.Text -> Maybe T.Text
stripToWord w = let w' = T.strip . T.filter (`notElem` ".,-") $ w
                in if T.length w' > 1 then Just w' else Nothing

tokenize :: T.Text -> [T.Text]
tokenize s = catMaybes . map stripToWord . T.words $ s

searchDocs :: Int -> Map.Map DocumentId Document -> T.Text -> [Document]
searchDocs n docs queryText = let
  queryTokens = tokenize queryText
  dTokens d = tokenize (docTitle d) ++ (catMaybes $ map stripToWord (docAuthors d))
  dScore d = length . filter (`elem` queryTokens) $ dTokens d
  in filter ((>0) . dScore) . take n . sortBy (\a b -> dScore a `compare` dScore b) $ Map.elems docs