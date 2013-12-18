module Reffit.Sort where

import Reffit.Types
import Reffit.Scores

import Data.List

-- |Sort doc list by a function - bool to False for ascending order
--  Highest-scoring paper will be at head of list
sortDocs :: (Document -> Int) -> Bool -> [Document]  -> [Document]
sortDocs scoreF True  = sortBy (\a b -> scoreF b `compare` scoreF a)
sortDocs scoreF False = sortBy (\a b -> scoreF a `compare` scoreF b)