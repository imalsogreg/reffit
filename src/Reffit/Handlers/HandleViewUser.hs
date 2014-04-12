{-# LANGUAGE OverloadedStrings #-}

module Reffit.Handlers.HandleViewUser (
      handleViewUser
    , handleFollow
    , handlePin
    , handleAddTag) where

import Reffit.Types
import Reffit.AcidTypes
import Reffit.Document
import Reffit.OverviewComment
import Reffit.User
import Reffit.PaperRoll
import Reffit.Sort
import Reffit.FieldTag
import Reffit.Scores

import qualified Text.XmlHtml as X
import Safe
import Control.Applicative ((<$>),(<*>),pure)
import Control.Monad.Trans

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time
import Snap.Core (getParam, redirect, writeBS)
import Snap.Types (writeText)
import Snap.Snaplet (Handler)
import Snap.Snaplet.AcidState (query,update)
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Monoid (mempty, (<>))

userPinboardDocs :: Map.Map DocumentId Document -> User -> [Document]
userPinboardDocs docs =
  catMaybes . Prelude.map (\dId -> Map.lookup dId docs) .
  Set.toList . _userPinboard

handleFollow :: Bool -> Handler App (AuthManager App) ()
handleFollow doFollow = do
  aUser <- currentUser
  us <- query QueryAllUsers
  fUser' <- getParam "username"
  t      <- liftIO $ getCurrentTime
  case join $ ((\uName -> Map.lookup uName us) . userLogin) <$> aUser of
    Nothing -> redirect "#"
    Just thisUser -> case join $ ((\uName -> Map.lookup uName us) . decodeUtf8) <$> fUser' of
      Nothing -> writeBS "username Param error" -- TODO error page
      Just fUser -> let toPath = encodeUtf8 $ T.append "/user/" (_userName fUser) in
        case doFollow of
             True  -> update (UserFollow   thisUser fUser t) >>= \_ -> redirect toPath
             False -> update (UserUnfollow thisUser fUser  ) >>= \_ -> redirect toPath

handlePin :: Bool -> Handler App (AuthManager App) ()
handlePin doPin = do
  aUser <- currentUser
  us <- query QueryAllUsers
  dId'  <- getParam "paperid"
  t     <- liftIO $ getCurrentTime
  case join $ ((\uName -> Map.lookup uName us) . userLogin) <$> aUser of
    Nothing -> redirect "#"
    Just user -> case join $ (readMay . T.unpack . decodeUtf8) <$> dId' of
      Nothing -> writeBS "Param error" --TODO error page
      Just dId -> do
        _ <- update $ Pin user dId doPin t
        redirect . BS.pack $ "/view_article/" ++ (show dId)

handleViewUser :: Handler App (AuthManager App) ()
handleViewUser = do
  userMap <- query QueryAllUsers
  docs    <- query QueryAllDocs
  cAUser' <- currentUser
  profileName' <- getParam "username"
  t       <- liftIO $ getCurrentTime
  case decodeUtf8 <$> profileName' of
    Nothing -> writeText "Error decoding username"  --TODO
    Just profileName -> case Map.lookup profileName userMap of
      Nothing -> writeText "User not in database."  -- TODO
      Just profileUser -> do
        let cUser' = join $ Map.lookup <$> (userLogin <$> cAUser') <*> pure userMap :: Maybe User
        renderWithSplices "user" (profileSplices t cUser' profileUser docs)

handleAddTag :: Bool -> Handler App (AuthManager App) ()
handleAddTag doAdd = do
  aUser <- currentUser
  us    <- query QueryAllUsers
  tags  <- query QueryAllFieldTags
  tag   <- getParam "fieldtag"
  case (fromFullName . T.decodeUtf8) <$> tag of
    Nothing -> writeBS "error reading fieldtag parameter" --TODO error page
    Just tp -> do
      when (not (tp `tagPathIsElem` tags) && doAdd)
               $ writeBS "fieldtag wasn't in fieldtag database"
      case join $ Map.lookup <$> (userLogin <$> aUser) <*> pure us of
        Nothing -> writeBS "error user isn't in database"
        Just user -> do
          _ <- if doAdd
            then update $ AddUserTag user tp
            else update $ DeleteUserTag user tp
          redirect "/"


profileSplices :: UTCTime -> Maybe User -> User -> Map.Map DocumentId Document
               -> Splices (SnapletISplice App)
profileSplices t cUser' profileUser docs = do
  -- Conditionally splice OUT the 'follow button'
  -- TODO this is a temporary measure to prevent self-following.
  case cUser' of
    Just liUser -> do
      "userRep"      ## I.textSplice . T.pack . show $
        userReputation docs liUser
    Nothing -> mempty
  when (cUser' == Just profileUser) $ 
    "followButton" ## I.textSplice ""
  "userName"      ## I.textSplice $ _userName profileUser
  "userRealName"  ## I.textSplice $ _userRealName profileUser
  "profileRep"    ## I.textSplice . T.pack . show $
    userReputation docs profileUser
  "followBtnText" ## I.textSplice followBtnText
  "followBtnLink" ## I.textSplice followBtnLink
  (allEventSplices  t docs (_userHistory profileUser))
  "nPinboard"     ## I.textSplice . T.pack . show .
    length $ userPinboardDocs docs profileUser
  "nFollowing"    ## I.textSplice . T.pack . show . Set.size . _userFollowing
    $ profileUser
  "nFollowers"    ## I.textSplice . T.pack . show . Set.size . _userFollowedBy
    $ profileUser
  (allPaperRollSplices $ userPinboardDocs docs profileUser)
  (allUserBlockSplices "following" docs (_userFollowing  profileUser))
  (allUserBlockSplices "followers" docs (_userFollowedBy profileUser))
  where (followBtnText,followBtnLink) =
          case Set.member (_userName profileUser) <$> (_userFollowing <$> cUser') of
            Just False -> ("Follow",   T.append "follow/"  (_userName profileUser))
            Just True  -> ("Unfollow", T.append "unfollow/" (_userName profileUser))
            Nothing    -> ("n/a","/")

allEventSplices :: UTCTime -> Map.Map DocumentId Document
                   -> [UserEvent] -> Splices (SnapletISplice App)
allEventSplices t docs events = do
  "userEvents" ## renderEvents t docs events

renderEvents :: UTCTime -> Map.Map DocumentId Document
                -> [UserEvent] -> SnapletISplice App
renderEvents t docs = I.mapSplices $ I.runChildrenWith . splicesFromEvent t docs

splicesFromEvent :: (Monad n) => UTCTime -> Map.Map DocumentId Document
                    ->UserEvent -> Splices (I.Splice n)

splicesFromEvent t docs event = do
    "eventNode" ##  eventSplice t docs event

eventSplice :: (Monad m) => UTCTime -> Map.Map DocumentId Document
               -> UserEvent -> I.Splice m
eventSplice t docs (WroteOComment dId cId)  =
  return [X.Element "p" [] [X.TextNode  "commented on "
                           , X.Element  "a" [("href",dLinkT dId docs)]
                             [X.TextNode . shortTitle tLen $ dTitleT dId docs]
                           , X.TextNode $ ocTimeT t dId cId docs] ]
eventSplice _ _ (VotedOnOComment _ _ _ _) = return []

{-
eventSplice t docs (WroteSummary dId sId) =
  return [X.Element "p" [] [X.TextNode "write a summary of "
                           ,X.Element "a" [("href",dLinkT dId docs)]
                            [X.TextNode . shortTitle tLen $ dTitleT dId docs]
                           ,X.TextNode $ sTimeT t dId sId docs] ]
eventSplice _ _ (VotedOnSummary _ _ _ _) = return []
-}

eventSplice t docs (PostedDocument dId) =
  return [X.Element "p" [] [X.TextNode "posted "
                           ,X.Element "a" [("href",dLinkT dId docs)]
                            [X.TextNode . shortTitle tLen $ dTitleT dId docs]
                           ,X.TextNode $ dTimeT t dId docs]]
eventSplice t _ (FollowedUser uName eTime) =
  return [X.Element "p" [] [X.TextNode "followed "
                           ,X.Element "a" [("href", T.append "/user/" uName)]
                            [X.TextNode uName]
                           ,X.TextNode . T.pack . sayTimeDiff t $ eTime]]
eventSplice t docs (PinnedDoc dId eTime) =
  return [X.Element "p" [] [X.TextNode "pinned "
                           ,X.Element "a" [("href",dLinkT dId docs)]
                            [X.TextNode . shortTitle tLen $ dTitleT dId docs]
                           ,X.TextNode . T.pack $ sayTimeDiff t eTime]]

tLen :: Int
tLen = 60

shortTitle :: Int -> T.Text -> T.Text
shortTitle n t
  | T.length t <= n = t
  | otherwise       = sTitle
  where
    tokens        = T.splitOn " " t :: [T.Text]
    tLengths      = scanl (\s tk -> s + T.length tk) 0 tokens :: [Int]
    tLengthsS     = zipWith (+) tLengths [0..] --lngth of titles,counting spaces
    measureTokens = zip tLengthsS tokens
    keepTokens    = takeWhile ((< n - 1) . fst) measureTokens :: [(Int,T.Text)]
    sTitle        = T.append (T.intercalate " " (map snd keepTokens)) "..." :: T.Text

dTitleT :: DocumentId -> Map.Map DocumentId Document -> T.Text
dTitleT dId docs = maybe "error" docTitle (Map.lookup dId docs)

dLinkT :: DocumentId -> Map.Map DocumentId Document -> T.Text
dLinkT dId _    = T.append "/view_article/" (T.pack . show $ dId)
--dLinkT dId docs = maybe "#" (T.append "/view_article/") (docLink <$> Map.lookup dId docs)

dTimeT :: UTCTime -> DocumentId -> Map.Map DocumentId Document -> T.Text
dTimeT t dId docs = maybe "error" T.pack $ do
  doc <- Map.lookup dId docs
  return $ sayTimeDiff t (docPostTime doc)

ocTimeT :: UTCTime -> DocumentId -> OverviewCommentId
           -> Map.Map DocumentId Document
           -> T.Text
ocTimeT t dId cId docs = maybe "error" T.pack $ do
  doc  <- Map.lookup dId docs
  comm <- Map.lookup cId $ docOComments doc
  return $ sayTimeDiff t (ocPostTime comm)

allUserBlockSplices :: T.Text -> Map.Map DocumentId Document
                       -> Set.Set UserName -> Splices (SnapletISplice App)
allUserBlockSplices nodeName docs userNames =
  nodeName ## renderUserBlocks docs (Set.toList userNames)

renderUserBlocks :: Map.Map DocumentId Document -> [UserName] -> SnapletISplice App
renderUserBlocks docs = I.mapSplices $ I.runChildrenWith . splicesFromUserBlock docs

-- TODO - put users reputations in the parens (will have to take users map,
--        or get the User in here, not just UserName
splicesFromUserBlock :: Monad n => Map.Map DocumentId Document
                        -> UserName -> Splices (I.Splice n)
splicesFromUserBlock docs userName = do
  "userName" ## I.textSplice userName
