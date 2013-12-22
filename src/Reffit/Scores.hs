module Reffit.Scores where

import Reffit.Types

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Time.Clock
import Data.Maybe

type ProseScore = Int

summaryScores :: Summary -> (Int,Int)
summaryScores s =
  let (upVotes,downVotes) = List.partition (==UpVote) $ summaryVotes s
  in (length upVotes, length downVotes)

critiqueScores :: Critique -> (Int,Int)
critiqueScores c = let (upVotes, downVotes) 
                         = List.partition (==UpVote) $ critiqueReactions c
                  in (length upVotes, length downVotes)

type NoveltyScore  = Int
type RigorScore    = Int
type CoolnessScore = Int

documentDimScores :: Document -> (NoveltyScore, RigorScore, CoolnessScore)
documentDimScores doc =
  let filtQuality         = filter ((\(u,d) -> u-d >1) . critiqueScores)
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

hotnessScore ::  UTCTime ->Document -> Int
hotnessScore now doc = qualityScore doc -
                       (floor $ ptsPerSec * secsOld)
  where
    secsOld = toRational $ diffUTCTime now (docPostTime doc) :: Rational
    ptsPerSec = 0.001 :: Rational
    
controversyScore :: Document -> Int
controversyScore doc = 
  let (p,c) = documentNCritiques doc
  in (p + c) * (10 - (abs $ p - c))   -- TODO this is a pretty weak controversy score 


lookupSummary :: DocumentId -> SummaryId -> Map.Map DocumentId Document 
                 -> Maybe Summary
lookupSummary dId sId docs = do
  doc <- Map.lookup dId docs
  Map.lookup sId $ docSummaries doc
  
lookupSummaries :: [(DocumentId, SummaryId)] -> Map.Map DocumentId Document
                   -> [Summary]
lookupSummaries inds docs = catMaybes 
                            [lookupSummary dId sId docs | (dId,sId) <- inds]
  
lookupCritique :: DocumentId -> CritiqueId -> Map.Map DocumentId Document 
                  -> Maybe Critique
lookupCritique dId cId docs = do
  doc <- Map.lookup dId docs
  Map.lookup cId $ docCritiques doc

lookupCritiques :: [(DocumentId,CritiqueId)] -> Map.Map DocumentId Document
                   -> [Critique]
lookupCritiques inds docs = catMaybes
                            [lookupCritique dId cId docs | (dId,cId) <- inds]

data UserStats = UserStats { userNPosts     :: Int
                           , userSummaries  :: (Int,Int,Int)
                           , userPraises    :: (Int,Int,Int)
                           , userCriticisms :: (Int,Int,Int)
                           , userNVotes     :: (Int,Int)
                           } deriving (Eq, Show)

-- TODO : This isn't too nice on the eyes
userUsageStats :: Map.Map DocumentId Document -> User -> UserStats
userUsageStats docs user = UserStats 
                           nDoc 
                           (nSum,sumUp,sumDown)
                           (nPraises,nPUps,nPDowns)
                           (nCrits,  nCUps,nCDowns)
                           (nUp,nDown)
  where
    h = userHistory user
    nDoc = length [x | x@(PostedDocument {}) <- h]
    sums  = catMaybes [lookupSummary dId sId docs | (WroteSummary dId sId) <- h]
    nSum  = length sums
    (sumUp,sumDown) = foldl (\(a,b) (c,d) -> (a+b,c+d)) (0,0) (map summaryScores sums)
    critiqueInds = [(dId,cId) | (WroteCritique dId cId) <- h]
    critiques = lookupCritiques critiqueInds docs
    (praises,crits) = List.partition ((==UpVote) . critiqueVal) critiques
    nPraises     = length praises
    praiseVotes = concat . map critiqueReactions $ praises
    (pUps, pDowns) = List.partition (==UpVote) praiseVotes
    (nPUps,nPDowns) = (length pUps, length pDowns)
    nCrits = length crits
    critVotes = concat . map critiqueReactions $ crits
    (cUps,cDowns) = List.partition (==UpVote) critVotes
    (nCUps,nCDowns) = (length cUps, length cDowns)
    allUserVotes = catMaybes $ [vd|(VotedOnSummary _ _ vd _)  <- h] ++
                   [vd|(VotedOnCritique _ _ vd _) <- h]
    (allUps,allDowns) = List.partition (==UpVote) allUserVotes
    (nUp,nDown) = (length allUps, length allDowns)
    
userReputation :: Map.Map DocumentId Document -> User -> Int
userReputation docs user =
  case userUsageStats docs user of
    (UserStats _ (_,sUp,_) (_,pUp,_) (_,cUp,_) _) ->
      sUp + pUp + cUp