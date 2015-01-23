{-# LANGUAGE OverloadedStrings #-}

module Reffit.Handlers.HandleViewPaper (handleViewPaper) where

import Reffit.Types
import Reffit.AcidTypes
import Reffit.Document
import Reffit.Markdown
import Reffit.OverviewComment
import Reffit.User
import Reffit.Sort
import Reffit.Scores

import Safe
import Control.Applicative ((<$>))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time
import Snap.Core (getParam)
import Snap.Core (writeText)
import Snap.Snaplet (Handler)
import Snap.Snaplet.AcidState (query)
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Text.Encoding (decodeUtf8)
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import qualified Data.Tree as Tree


------------------------------------------------------------------------------
handleViewPaper :: Handler App (AuthManager App) ()
handleViewPaper = do
  us    <- query QueryAllUsers
  docs  <- query QueryAllDocs
  pId'  <- getParam "paperid"
  aUser <- currentUser
  t     <- liftIO $ getCurrentTime
  let u = join $ (flip Map.lookup) us <$> userLogin <$> aUser  :: Maybe User
  case readMay . T.unpack . decodeUtf8 <$> pId' of
    Nothing -> writeText "Need paperid parameter"  -- TODO: Proper error page
    Just Nothing -> writeText "Just Nothing!  Odd."
    Just (Just pId) ->  do  -- just just again!
      docs <- query QueryAllDocs
      case Map.lookup pId docs of
        Nothing   -> writeText $
                     T.concat ["You entered: "
                              , T.pack (show pId)
                              ," Document wasn't found in the database."]
        Just doc -> renderWithSplices "_article_view" (allArticleViewSplices u us pId doc docs t)

--TODO Move all this score stuff into the Scores module

-- |Count positive and negative votes for a summary
summaryUpsDowns :: Summary -> (Int,Int)
summaryUpsDowns s = over both length $
  List.partition (==UpVote) . summaryVotes $ s

critiqueUpsDowns :: Critique -> (Int,Int)
critiqueUpsDowns c = over both length $
  List.partition (==UpVote) . critiqueReactions $ c

compareSummaryNetVote :: Summary -> Summary -> Ordering
compareSummaryNetVote a b = netA `compare` netB
  where (upA,downA) = summaryUpsDowns a
        netA = upA - downA
        (upB,downB) = summaryUpsDowns b
        netB = upB - downB

summarySummary :: Document -> T.Text
summarySummary doc =
  T.unwords [T.pack (show nVotes),"votes /"
            , T.pack (show  nSummaries), "summaries"]
  where nSummaries = documentNSummaries doc
        sums       = documentSummaries doc
        nVotes     = sum . map (length . ocResponse) . Map.elems $ sums

critiqueSummary :: Document -> UpDownVote -> T.Text
critiqueSummary doc critType =
  T.concat [T.pack (show concensusPct),"% consensus on "
           , T.pack (show $ (nUps+nDowns)), " points"]
  where
    (nUps,nDowns) = documentNCritiques doc
    concensusPct = if (nUps + nDowns > 0)
                   then floor (fI nUps/ fI (nUps+nDowns) * (100::Double))
                   else (0 :: Int)

fI :: (Integral a, Real b) => a -> b
fI = fromIntegral

-- This should be in Reffit.Scores
nSummaries :: Document -> Int
nSummaries doc = Map.size . Map.filter ((==Nothing).ocVote) . docOComments $ doc

-- This should be in Reffit.Scores
nCritique :: UpDownVote -> Document -> Int
nCritique vDir doc = Map.size . Map.filter ((==vDir) . snd . fromJust . ocVote)
                     . Map.filter ((/=Nothing) . ocVote)
                     $ docOComments doc

allArticleViewSplices :: Maybe User -> Map.Map UserName User -> DocumentId ->
                         Document -> Map.Map DocumentId Document ->
                         UTCTime -> Splices (SnapletISplice App)
allArticleViewSplices u us pId doc docs t = do
  "userRep"                 ## I.textSplice $ maybe ""
    (T.pack . show . userReputation docs) u
  "articleSummarySummary"   ## I.textSplice (summarySummary doc) :: Splices (SnapletISplice App)
  "articleURL"              ## I.textSplice (docLink doc)
  "articlePraiseSummary"    ## I.textSplice (critiqueSummary  doc UpVote) :: Splices (SnapletISplice App)
  "articleCriticismSummary" ## I.textSplice (critiqueSummary doc DownVote) :: Splices (SnapletISplice App)
  "nSummaries"              ## I.textSplice (T.pack . show $ nSummaries doc)
  "nPraise"                 ## I.textSplice (T.pack . show $ nCritique UpVote doc)
  "nCriticisms"             ## I.textSplice (T.pack . show $ nCritique DownVote doc)
  "docType"                 ## I.textSplice (docClassName . docClass $ doc)
  "docId"                   ## I.textSplice (T.pack . show $ docId doc)
  "docTitle"                ## I.textSplice (docTitle doc)
  "timeSince"               ## I.textSplice (T.pack . sayTimeDiff t . docPostTime $ doc)
  let (pinUrl, pinBtn) = pinText u
  "pinUrl"                  ## I.textSplice pinUrl
  "paperid"                 ## I.textSplice (T.pack . show $ pId)
  "pinboardBtnTxt"          ## I.textSplice pinBtn
  "discussionLink"          ## I.textSplice $ T.concat ["/view_discussion/?paperid="
                                                        ,T.pack . show . docId $ doc]
  "nDiscussionPoints"       ## I.textSplice .
    T.pack . show . length . concat . map Tree.flatten . docDiscussion $ doc
  (allOCommentSplices t Summary' "articleSummaries" u us doc docs)
  (allOCommentSplices t Praise "articlePraise" u us doc docs)
  (allOCommentSplices t Criticism "articleCriticisms" u us doc docs)
   where
     pinText :: Maybe User -> (T.Text, T.Text)
     pinText Nothing = ("","")
     pinText (Just user)
          | Set.member (docId doc) (userPinboard user) = ("unpin", "Unpin")
          | otherwise                                  = ("pin",   "Pinboard")


data UserProseRelation = UpVoted | DownVoted | AnonVoted | NotVoted

userCommentRelation :: User -> Document -> OverviewCommentId -> Maybe UserProseRelation
userCommentRelation u doc cId =
  let relevantActivity = [x | x@(VotedOnOComment dId' cId' _ _) <- userHistory u
                            , dId' == docId doc && cId == cId']
  in case relevantActivity of
    []                                       -> Just NotVoted
    [VotedOnOComment _ _ Nothing _ ]         -> Just AnonVoted
    [VotedOnOComment _ _ (Just UpVote) _]    -> Just UpVoted
    [VotedOnOComment _ _ (Just DownVote) _ ] -> Just DownVoted
    _ -> Nothing -- <- in these cases something went wrong w/ filtering,
                 -- or a doublevote happened

allOCommentSplices :: UTCTime -> OverviewCommentType -> T.Text
                      -> Maybe User -> Map.Map UserName User
                      -> Document -> Map.Map DocumentId Document
                      -> Splices (SnapletISplice App)
allOCommentSplices t commType tagText u us doc docs =
  tagText ## renderOComments t commType u us doc docs (Map.toList cs')
 where (p,c) = documentCritiques doc
       cs' = case commType of
         Summary'  -> documentSummaries doc
         Praise    -> p
         Criticism -> c

renderOComments :: UTCTime -> OverviewCommentType ->
                   Maybe User -> Map.Map UserName User ->
                   Document -> Map.Map DocumentId Document ->
                   [(OverviewCommentId, OverviewComment)]
                   -> SnapletISplice App
renderOComments t ct u us doc docs = I.mapSplices $ I.runChildrenWith .
                                  splicesFromOComment t ct u us doc docs

splicesFromOComment :: Monad n => UTCTime -> OverviewCommentType
                       -> Maybe User -> Map.Map UserName User
                       -> Document -> Map.Map DocumentId Document
                       -> (OverviewCommentId, OverviewComment)
                       -> Splices (I.Splice n)
splicesFromOComment t ct viewingU us doc docs (cId,c) = do
  "upCount"      ## I.textSplice (T.pack . show $ nUp)
  "downCount"    ## I.textSplice (T.pack . show $ nDown)
  "proseText"    ## markdownSplice (ocText c)
  reBlock
  "proseTimeSince"    ## I.textSplice (T.pack . sayTimeDiff t . ocPostTime $ c)
  "discussionUrl" ## I.textSplice (T.concat ["/view_discussion/?paperid="
                                            ,T.pack . show $ docId doc
                                            ,"&amp;commentid="
                                            ,T.pack . show $ cId
                                             ])
  "nDiscussionPoints" ## I.textSplice .
    T.pack . show . length . concat . map Tree.flatten . ocDiscussion $ c
  case ocPoster c of
    Nothing -> do
      "prosePoster"            ## I.textSplice "Anonymous"
      "prosePosterDestination" ## I.textSplice "#"
    Just uName -> do
      "prosePoster"            ## I.textSplice $ maybe "unknown" userText lookupUName
      "prosePosterDestination" ## I.textSplice $ T.append "/user/" uName
      where
        userText u  = T.concat [userName u," (", T.pack . show $ userReputation docs u,")"]
        lookupUName = Map.lookup uName us
  case viewingU of
    Nothing -> do
      "upBtnUrl"   ## I.textSplice "#"
      "downBtnUrl" ## I.textSplice "#"
      "editBlock"  ## I.textSplice ""
    Just user -> do
      when (Just (userName user) /= ocPoster c)
        ("editBlock" ## I.textSplice "")
      when (Just (userName user) == ocPoster c) $ do
        let commTypeStr = case ocVote c of
              Nothing ->           "summary"
              Just (_,DownVote) -> "criticism"
              Just (_,UpVote)   -> "praise"
            pIdText = T.pack . show . docId $ doc :: T.Text
            cIdText = T.pack . show $ cId :: T.Text
        "editURL" ## I.textSplice (T.concat ["new_", commTypeStr, "?paperid=", pIdText, "&amp;commentid=", cIdText])
      case userCommentRelation user doc cId of
        (Just AnonVoted) -> do
          "upBtnUrl" ## I.textSplice "#"
          "downBtnUrl" ## I.textSplice "#"
          "upBtnHighlight" ## I.textSplice   "triangle-anon"
          "downBtnHighlight" ## I.textSplice "triangle-anon"
        (Just UpVoted) -> do
          "upBtnUrl" ## I.textSplice "#"
          "downBtnUrl" ## I.textSplice "#"
          "upBtnHighlight" ## I.textSplice "triangle-on"
          "downBtnHighlight" ## I.textSplice "triangle-off"
        (Just DownVoted) -> do
          "upBtnUrl" ## I.textSplice "#"
          "downBtnUrl" ## I.textSplice "#"
          "upBtnHighlight" ## I.textSplice "triangle-off"
          "downBtnHighlight" ## I.textSplice "triangle-on"
        (Just NotVoted) -> do
          "upBtnUrl" ## I.textSplice (T.concat ["/cast_ocomment_upvote/"
                                               ,T.pack (show $ docId doc)
                                               ,"."
                                               ,T.pack (show $ cId)])
          "downBtnUrl" ## I.textSplice (T.concat ["/cast_ocomment_downvote/"
                                                 ,T.pack (show $ docId doc)
                                                 ,"."
                                                 ,T.pack (show $ cId)])
          "upBtnHighlight" ## I.textSplice "triangle-togglable"
          "downBtnHighlight" ## I.textSplice "triangle-togglable"
        Nothing -> return ()
  where
    reBlock = case ct of
--      Summary' -> "reBlock" ## I.textSplice . T.pack . show $ Summary'
--      _        -> "reBlock" ## I.textSplice . T.pack . show $ ct
      Summary' -> "reBlock" ## I.textSplice "" -- chop out the 're:' block
      _        -> "critiqueDim" ##
                  I.textSplice (T.pack . show . fst .
                                fromJust . ocVote $ c)
    (nUp,nDown) = commentScores c
