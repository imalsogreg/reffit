{-# LANGUAGE OverloadedStrings #-}

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

testTags :: Forest FieldTag
testTags =
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
    , Node "Physics" [
       Node "TheoreticalPhysics" [] 
       , Node "HighEnergyPhysics" []
       , Node "NuclearPhysics" []
       ]
    ]

topLabels :: FieldTags -> [T.Text]
topLabels fts = map (\(Node t _) -> t) fts

tagPathIsElem :: TagPath -> FieldTags -> Bool
tagPathIsElem tp tagsTop = not (L.null tp) && aux tp tagsTop
  where
    aux [] _ = True
    aux (t:ts) tags = case L.elemIndex t (topLabels tags) of
      Nothing -> False
      Just ix -> let (Node _ tags') = tags !! ix in
          tagPathIsElem ts tags'

{- This is wrong
tagPaths :: FieldTags -> [TagPath]
tagPaths [] = []
tagPaths ((Node tagT subTags):xs) = [tagT] : map (tagT:) (tagPaths subTags)
-}

fieldTagsToStringForest :: FieldTags -> Forest String
fieldTagsToStringForest [] = []
fieldTagsToStringForest (n:ns) = tagTreeToStringTree n : fieldTagsToStringForest ns

tagTreeToStringTree :: Tree FieldTag -> Tree String
tagTreeToStringTree (Node t frst) = Node (T.unpack t)
                                    $ fieldTagsToStringForest frst

drawFieldTags :: FieldTags -> String
drawFieldTags = drawForest .  fieldTagsToStringForest

--TODO make splices for FieldTags

