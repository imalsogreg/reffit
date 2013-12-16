module Reffit.Scores where

import Reffit.Types

import qualified Data.Map as Map
import qualified Data.List as List

type ProseScore = Int

summaryScore :: Summary -> ProseScore
summaryScore s =
  let (upVotes,downVotes) = List.partition (==UpVote) $ summaryVotes s
  in length upVotes - length downVotes

critiqueScore :: Critique -> ProseScore
critiqueScore c = let (upVotes, downVotes) = List.partition (==UpVote) $ critiqueReactions c
                  in length upVotes - length downVotes

type NoveltyScore  = Int
type RigorScore    = Int
type CoolnessScore = Int

documentScores :: Document -> (NoveltyScore, RigorScore, CoolnessScore)
documentScores doc =
  let filtQuality         = filter ((>1) . critiqueScore)
      filtDim d           = filter ((==d) . critiqueDim)
      filtPraise          = filter ((==UpVote)   . critiqueVal)
      filtCriticism       = filter ((==DownVote) . critiqueVal)
      listCs               = Map.elems $ docCritiques doc
      dimensionScore d    = (length . filtDim d . filtQuality $ filtPraise    listCs) -
                            (length . filtDim d . filtQuality $ filtCriticism listCs)
  in (dimensionScore Novelty, dimensionScore Rigor, dimensionScore Coolness)