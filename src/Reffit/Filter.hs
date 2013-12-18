module Reffit.Filter where

import Reffit.Types
import Reffit.FieldTag

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Tree as Tree
import Data.Time

filterDocs :: [TagPath] -> [DocClass] -> [Document] -> [Document]
filterDocs queryTags queryClasses =
  let docTagsMatch d  = any (\dTag ->
                              any (\qTag -> dTag `tagIncludes` qTag) queryTags)
                        (docFieldTags d)
      docClassMatch d = any (== docClass d) queryClasses
  in  filter docTagsMatch . filter docClassMatch


