{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Reffit.FieldTag where

import qualified Data.Text as T
import qualified Data.List as L
import Data.Tree
import Data.String

import Heist
import qualified Heist.Interpreted as I
import Snap.Snaplet.Heist

type FieldTag = T.Text
type TagPath = [FieldTag]
type FieldTags = Forest FieldTag

tagHierarchy :: Forest FieldTag
tagHierarchy =
  [
    Node "Biology" [
         Node "Anthropology" []
       , Node "Cancer" []
       , Node "Medicine" []
       , Node "Microbiology" []
       , Node "Neuroscience" []
       ]
    , Node "Chemistry" [
         Node "OrganicChemistry" []
       , Node "InorganicChemistry" []
       ]
    , Node "ComputerScience" [
         Node "Algorithms" []
       , Node "Compilers" [] 
       , Node "FunctionalProgramming" [] 
       , Node "Verification" [] 
       ]
    , Node "Math" [
         Node "AppliedMath" [] 
       , Node "TheoreticalMath" []
       ]
    , Node "Philosophy" [
       Node "PhilosophyOfScience" []
       , Node "PhilosophyOfMind" []
       , Node "Epistemology" []
       , Node "Ethics" []
       ]
    , Node "Physics" [
       Node "TheoreticalPhysics" [] 
       , Node "HighEnergyPhysics" []
       , Node "NuclearPhysics" []
       ]
    ]

topLabels :: FieldTags -> [T.Text]
topLabels fts = map (\(Node t _) -> t) fts

toFullName :: TagPath -> T.Text
toFullName = T.intercalate "."

fromFullName :: T.Text -> TagPath
fromFullName = T.splitOn "."

tagPathIsElem :: TagPath -> FieldTags -> Bool
tagPathIsElem tp tagsTop = not (L.null tp) && aux tp tagsTop
  where
    aux [] _ = True
    aux (t:ts) tags = case L.elemIndex t (topLabels tags) of
      Nothing -> False
      Just ix -> let (Node _ tags') = tags !! ix in
          aux ts tags'

insertTag :: TagPath -> FieldTags -> FieldTags
insertTag [] tags = tags
insertTag (x:xs) [] = [Node x (insertTag xs [])]
insertTag (x:xs) tags = case L.elemIndex x (topLabels tags) of
  Nothing -> (Node x (insertTag xs [])) : tags
  Just ix ->
    let (preList,((Node _ tags'):postList)) = splitAt ix tags in
    preList ++ [Node x (insertTag xs tags')] ++ postList 

-- |Determine whether a reference path 'includes' a query path
--  e.g. a document with "Biology"."Neuroscience" should be
--  included in a query for "Biology"
  
--TODO need to clarify the meaning, better name.  which tag is first?
tagIncludes :: TagPath -> TagPath -> Bool
_  `tagIncludes` []     = True                     -- Query is general
[] `tagIncludes` (_:_)  = False                    -- Query is specific
referenceTag `tagIncludes` queryTag
  | length queryTag > length referenceTag = False  -- Query is specific
(refTag:rps) `tagIncludes` (qurTag:qrs)
  | refTag == qurTag  = rps `tagIncludes` qrs
  | otherwise          = False


showPath :: TagPath -> String
showPath tp = T.unpack $ T.intercalate "." tp

fieldTagsToStringForest :: FieldTags -> Forest String
fieldTagsToStringForest [] = []
fieldTagsToStringForest (n:ns) = tagTreeToStringTree n : fieldTagsToStringForest ns

tagTreeToStringTree :: Tree FieldTag -> Tree String
tagTreeToStringTree (Node t frst) = Node (T.unpack t)
                                    $ fieldTagsToStringForest frst

drawFieldTags :: FieldTags -> String
drawFieldTags = drawForest .  fieldTagsToStringForest

--TODO make splices for FieldTags

