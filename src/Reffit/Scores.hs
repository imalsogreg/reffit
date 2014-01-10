module Reffit.Scores where

import Reffit.Types
import Reffit.Document
import Reffit.User
import Reffit.OverviewComment

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

commentScores :: OverviewComment -> (Int,Int)
commentScores c =
  let (upVotes,downVotes) = List.partition (==UpVote) $ ocResponse c
  in  (length upVotes, length downVotes)

type NoveltyScore  = Int
type RigorScore    = Int
type CoolnessScore = Int

documentDimScores :: Document -> (NoveltyScore, RigorScore, CoolnessScore)
documentDimScores doc =
  let filtQuality         = filter ((\(u,d) -> u-d >1) . commentScores) :: [OverviewComment] -> [OverviewComment]
      filtDim d           = filter ((==d) . fst . fromJust . ocVote)
      filtPraise          = filter ((==UpVote)   . snd . fromJust . ocVote)
      filtCriticism       = filter ((==DownVote) . snd . fromJust . ocVote)
      listCritiques       = filter ((/=Nothing) . ocVote) . Map.elems $ docOComments doc
      dimensionScore d    = (length . filtDim d . filtQuality $ filtPraise    listCritiques) -
                            (length . filtDim d . filtQuality $ filtCriticism listCritiques)
  in (dimensionScore Novelty, dimensionScore Rigor, dimensionScore Coolness)

documentSummaries :: Document -> Map.Map OverviewCommentId OverviewComment
documentSummaries = Map.filter ((==Nothing) . ocVote) . docOComments

documentNSummaries :: Document -> Int
documentNSummaries = Map.size . documentSummaries

documentCritiques :: Document -> (Map.Map OverviewCommentId OverviewComment
                                 ,Map.Map OverviewCommentId OverviewComment)
documentCritiques doc = Map.partition ((==UpVote) . snd . fromJust . ocVote) .
                        Map.filter ((/=Nothing) . ocVote) . docOComments $ doc

documentNCritiques :: Document -> (Int,Int)
documentNCritiques d = (length praise, length criticism)
  where
    critiques = List.filter ((/=Nothing).ocVote) . Map.elems $ docOComments d
    (praise,criticism) = List.partition ((==UpVote) . snd . fromJust . ocVote) critiques

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


lookupOComment :: DocumentId -> OverviewCommentId -> Map.Map DocumentId Document
               -> Maybe OverviewComment
lookupOComment dId cId docs = do
  doc <- Map.lookup dId docs
  Map.lookup cId $ docOComments doc

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
    comments = catMaybes [lookupOComment dId sId docs | (WroteOComment dId sId) <- h]
    sums = filter ((==Nothing) . ocVote) comments
    nSum  = length sums
    (sumUp,sumDown) = foldl (\(a,b) (c,d) -> (a+b,c+d)) (0,0) (map commentScores sums)
    critiques = filter ((/=Nothing).ocVote) comments
    (praises,crits) = List.partition ((==UpVote) . snd . fromJust . ocVote) critiques
    nPraises     = length praises
    praiseVotes = concat . map ocResponse $ praises
    (pUps, pDowns) = List.partition (==UpVote) praiseVotes
    (nPUps,nPDowns) = (length pUps, length pDowns)
    nCrits = length crits
    critVotes = concat . map ocResponse $ crits
    (cUps,cDowns) = List.partition (==UpVote) critVotes
    (nCUps,nCDowns) = (length cUps, length cDowns)
    allUserVotes = catMaybes $ [vd|(VotedOnOComment _ _ vd _)  <- h]
    (allUps,allDowns) = List.partition (==UpVote) allUserVotes
    (nUp,nDown) = (length allUps, length allDowns)

userReputation :: Map.Map DocumentId Document -> User -> Int
userReputation docs user =
  case userUsageStats docs user of
    (UserStats _ (_,sUp,_) (_,pUp,_) (_,cUp,_) _) ->
      sUp + pUp + cUp