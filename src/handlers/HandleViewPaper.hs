{-# LANGUAGE OverloadedStrings #-}

module HandleViewPaper (handleViewPaper) where

import Reffit.Types
import Reffit.AcidTypes

import Safe
import Control.Applicative ((<$>),(<*>),pure)
import qualified Data.List as List
import qualified Data.Map as Map
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
   
handleViewPaper :: Handler App (AuthManager App) ()
handleViewPaper = do
  us <- query QueryAllUsers
  pId'  <- getParam "paperid"
  aUser <- currentUser
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
        Just doc -> renderWithSplices "_article_view" (allArticleViewSplices u doc)        

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
  where nSummaries = Map.size . docSummaries $ doc
        nVotes     = sum . map length . map summaryVotes . Map.elems . docSummaries $ doc :: Int

critiqueSummary :: Document -> UpDownVote -> T.Text
critiqueSummary doc critType = 
  T.concat [T.pack (show concensusPct),"% consensus on "
           , T.pack (show . length $ critiques), " points"] 
  where 
    critiques = filter ((==critType).critiqueVal) . Map.elems . docCritiques $ doc :: [Critique]
    (nUps,nDowns) = List.partition ((==critType).critiqueVal) critiques & over both length
    concensusPct = if (nUps + nDowns > 0)
                   then floor (fI nUps/ fI (nUps+nDowns) * (100::Double)) 
                   else (0 :: Int)

fI :: (Integral a, Real b) => a -> b
fI = fromIntegral

nSummaries :: Document -> Int
nSummaries doc = Map.size $ docSummaries doc

nCritique :: UpDownVote -> Document -> Int
nCritique vDir doc = Map.size . Map.filter ((==vDir) . critiqueVal)
                     $ docCritiques doc
  
allArticleViewSplices :: Maybe User -> Document -> Splices (SnapletISplice App)
allArticleViewSplices u doc = do
  "articleSummarySummary"   ## I.textSplice (summarySummary doc) :: Splices (SnapletISplice App)
  "articlePraiseSummary"    ## I.textSplice (critiqueSummary  doc UpVote) :: Splices (SnapletISplice App)
  "articleCriticismSummary" ## I.textSplice (critiqueSummary doc DownVote) :: Splices (SnapletISplice App)
  "nSummaries"              ## I.textSplice (T.pack . show $ nSummaries doc)
  "nPraise"                 ## I.textSplice (T.pack . show $ nCritique UpVote doc)
  "nCriticisms"             ## I.textSplice (T.pack . show $ nCritique DownVote doc)
  "docType"                 ## I.textSplice (docClassName . docClass $ doc)  
  "docId"                   ## I.textSplice (T.pack . show $ docId doc)
  "docTitle"                ## I.textSplice (docTitle doc)
  let (pinUrl, pinBtn) = pinText u
  "pinUrl"                  ## I.textSplice pinUrl
  "pinboardBtnTxt"          ## I.textSplice pinBtn
  (allSummarySplices u doc . Map.toList . docSummaries $ doc)
  (allCritiqueSplices UpVote   "articlePraise"     u doc . Map.toList . docCritiques $ doc )
  (allCritiqueSplices DownVote "articleCriticisms" u doc . Map.toList . docCritiques $ doc )
   where
     pinText :: Maybe User -> (T.Text, T.Text)
     pinText Nothing = ("","")
     pinText (Just user)
          | Set.member (docId doc) (userPinboard user) = ("unpin", "Unpin")
          | otherwise                                  = ("pin",   "Pinboard")
                         
allSummarySplices :: Maybe User -> Document -> [(SummaryId,Summary)] -> Splices (SnapletISplice App)
allSummarySplices u doc ss = "articleSummaries" ## renderSummaries u doc ss

renderSummaries :: Maybe User -> Document -> [(SummaryId,Summary)] -> SnapletISplice App
renderSummaries u doc = I.mapSplices $ I.runChildrenWith . splicesFromSummary u doc

splicesFromSummary :: Monad n => Maybe User -> Document -> (SummaryId,Summary) -> Splices (I.Splice n)
splicesFromSummary u doc (sId,s) = do 
  "upCount"       ## I.textSplice (T.pack . show $ nUp) 
  "downCount"     ## I.textSplice (T.pack . show $ nDown)
  "proseText"     ## I.textSplice (T.pack . show $ summaryProse s)
  case summaryPoster s of
    Nothing -> do
      "prosePoster"             ## I.textSplice "Anonymous"
      "prosePosterDestination"  ## I.textSplice "#"
    Just uName -> do
      "prosePoster"  ## I.textSplice uName
      "prosePosterDestination" ## I.textSplice $ T.append "/user/" uName
      
  case u of
    Nothing -> do
      "upBtnUrl"   ## I.textSplice "/login"
      "downBtnUrl" ## I.textSplice "/login"
    Just user ->
      case userSummaryRelation user doc sId of
        (Just AnonVoted) -> do
          "upBtnUrl" ## I.textSplice "#"
          "downBtnURl" ## I.textSplice "#"
          "upBtnHighlight" ## I.textSplice "triangle-anon"
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
          "upBtnUrl" ## I.textSplice (T.concat ["/cast_summary_upvote/"
                                               ,T.pack (show $ docId doc)
                                               ,"."
                                               ,T.pack (show $ sId)])
          "downBtnUrl" ## I.textSplice (T.concat ["/cast_summary_downvote/"
                                                 ,T.pack (show $ docId doc)
                                                 ,"."
                                                 ,T.pack (show $sId)])
          "upBtnHighlight" ## I.textSplice "triangle-togglable"
          "downBtnHighlight" ## I.textSplice "triangle-togglable"
        Nothing -> return ()  -- TODO this is some kind of problem,
                              -- couldn't find user -> summary rln
  where (nUp, nDown) = summaryUpsDowns s
                  
allCritiqueSplices :: UpDownVote -> T.Text -> Maybe User -> Document -> [(CritiqueId,Critique)]-> Splices (SnapletISplice App)
allCritiqueSplices vTarg tag u doc cs = tag ## renderCritiques u doc cs'
  where cs' = filter ( (==vTarg) . critiqueVal . snd) cs 

renderCritiques :: Maybe User -> Document -> [(CritiqueId,Critique)] -> SnapletISplice App
renderCritiques u doc = I.mapSplices $ I.runChildrenWith . splicesFromCritique u doc

splicesFromCritique :: Monad n => Maybe User -> Document -> (CritiqueId,Critique) -> Splices (I.Splice n)
splicesFromCritique u doc (cId,c) = do
  "upCount"      ## I.textSplice (T.pack . show $ nUp)
  "downCount"    ## I.textSplice (T.pack . show $ nDown)
  "proseText"    ## I.textSplice (critiqueProse c)
  "critiqueDim"  ## I.textSplice (T.pack . show $ critiqueDim c)
  case critiquePoster c of
    Nothing -> do
      "prosePoster"            ## I.textSplice "Anonymous"
      "prosePosterDestination" ## I.textSplice "#"
    Just uName -> do
      "prosePoster"            ## I.textSplice uName
      "prosePosterDestination" ## I.textSplice $ T.append "/user/" uName
  case u of
    Nothing -> do
      "upBtnUrl"   ## I.textSplice "/login"
      "downBtnUrl" ## I.textSplice "/login"
    Just user ->
      case userCritiqueRelation user doc cId of
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
          "upBtnUrl" ## I.textSplice (T.concat ["/cast_critique_upvote/"
                                               ,T.pack (show $ docId doc)
                                               ,"."
                                               ,T.pack (show $ cId)])
          "downBtnUrl" ## I.textSplice (T.concat ["/cast_critique_downvote/"
                                                 ,T.pack (show $ docId doc)
                                                 ,"."
                                                 ,T.pack (show $ cId)])
          "upBtnHighlight" ## I.textSplice "triangle-togglable"
          "downBtnHighlight" ## I.textSplice "triangle-togglable"
        Nothing -> return ()
  where
    (nUp,nDown) = critiqueUpsDowns c

data UserProseRelation = UpVoted | DownVoted | AnonVoted | NotVoted

userSummaryRelation :: User -> Document -> SummaryId -> Maybe UserProseRelation
userSummaryRelation u doc sId =
  let relevantActivity = [ x | x@(VotedOnSummary dId' sId' _) <- userHistory u
                             , dId' == docId doc &&  sId == sId']  
  in case relevantActivity of
    []                                   -> Just NotVoted
    [VotedOnSummary _ _ Nothing]         -> Just AnonVoted
    [VotedOnSummary _ _ (Just UpVote)]   -> Just UpVoted
    [VotedOnSummary _ _ (Just DownVote)] -> Just DownVoted
    _ -> Nothing  -- <- is these cases something went wrong w/ the filtering

userCritiqueRelation :: User -> Document -> CritiqueId -> Maybe UserProseRelation
userCritiqueRelation u doc cId =
  let relevantActivity = [ x | x@(VotedOnCritique dId' cId' _) <- userHistory u
                             , dId' == docId doc && cId == cId']
  in case relevantActivity of
    []                                    -> Just NotVoted
    [VotedOnCritique _ _ Nothing]         -> Just AnonVoted
    [VotedOnCritique _ _ (Just UpVote)]   -> Just UpVoted
    [VotedOnCritique _ _ (Just DownVote)] -> Just DownVoted
    _ -> Nothing -- <- multiple votes on the same critique!!
