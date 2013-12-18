module Reffit.Scores where

import Reffit.Types

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Time.Clock

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

documentDimScores :: Document -> (NoveltyScore, RigorScore, CoolnessScore)
documentDimScores doc =
  let filtQuality         = filter ((>1) . critiqueScore)
      filtDim d           = filter ((==d) . critiqueDim)
      filtPraise          = filter ((==UpVote)   . critiqueVal)
      filtCriticism       = filter ((==DownVote) . critiqueVal)
      listCs               = Map.elems $ docCritiques doc
      dimensionScore d    = (length . filtDim d . filtQuality $ filtPraise    listCs) -
                            (length . filtDim d . filtQuality $ filtCriticism listCs)
  in (dimensionScore Novelty, dimensionScore Rigor, dimensionScore Coolness)

documentNCritiques :: Document -> (Int,Int)
documentNCritiques d = (length praise, length criticism)
  where
    (praise,criticism) = List.partition ((==UpVote) . critiqueVal)
                         (Map.elems $ docCritiques d)

qualityScore :: Document -> Int
qualityScore doc = let (n,r,c) = documentDimScores doc in n + r + c

hotnessScore :: Document -> UTCTime -> Int
hotnessScore doc now = qualityScore doc -
                       (floor $ ptsPerSec * secsOld)
  where
    secsOld = toRational $ diffUTCTime now (docPostTime doc) :: Rational
    ptsPerSec = 0.1 :: Rational