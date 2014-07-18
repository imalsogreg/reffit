{-#LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Data.List

import qualified Data.Map as Map
import Data.Maybe
import Data.Serialize
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time
import Data.Tree
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int
import System.Environment

import Reffit.Document
import Reffit.Types
import Reffit.User
import Reffit.OverviewComment
import Reffit.Discussion
import Reffit.AcidTypes


------------------------------------------------------------------------------
insertUsers :: Connection -> PersistentState -> IO (Map.Map UserName Int)
insertUsers conn p = do
  zipWithM_ (f conn) [(0::Int)..] (Map.elems $ p^.users)
  insertFollowers conn (_users p) userIDs
  return userIDs
  where
    userIDs :: Map.Map UserName Int
    userIDs =  Map.fromList $ zip
               (map userName . Map.elems $ _users p) [(0::Int)..]
    f c i u = do
      _ <- execute' c "insert into reffitUsers (userid,username,userJoinTime) values (?,?,?)"
        (i, userName u :: T.Text, userJoinTime u)
      execute' conn
        "insert into emailaddys (emailID,emailAddy,userID,verified,isPrimary) values (?,?,?,?,?)"
        (i, userName u, i, False, True)


------------------------------------------------------------------------------
insertFollowers :: Connection
                -> Map.Map UserName User -> Map.Map UserName Int -> IO ()
insertFollowers conn rUsers userSqlIDs =
  forM_ (allFollowPairs) $ execute' conn
  "insert into userFollowers (follower,followed,followTime) values (?,?,?)"
  where
    allFollowPairs = concatMap userFollowed (Map.elems rUsers)

    userFollowed :: User -> [(Int,Int,UTCTime)]
    userFollowed u = catMaybes $
                     map (pairFollow u) (Set.toList $ userFollowing u)

    pairFollow :: User -> UserName -> Maybe (Int,Int,UTCTime)
    pairFollow u followed = (\x y z -> (x,y,z))
                            <$> Map.lookup (userName u) userSqlIDs
                            <*> Map.lookup followed     userSqlIDs
                            <*> pairFollowTime u followed

    pairFollowTime :: User -> UserName -> Maybe UTCTime
    pairFollowTime u followed = listToMaybe . catMaybes .
                                flip map (userHistory u) $
                                \h -> case h of
                                  FollowedUser n t -> if n == followed
                                                      then Just t
                                                      else Nothing
                                  _                -> Nothing


------------------------------------------------------------------------------
insertDocuments :: Connection -> PersistentState -> Map.Map UserName Int
                -> IO (Map.Map DocumentId Int, Map.Map OverviewCommentId Int)
insertDocuments conn p userIdMap = do
  let docIdMap = zip (map fst . Map.toList $ p^.documents) [(0::Int)..]
  commentIdMap <- concat <$> zipWithM (insertDocument conn p userIdMap )
    [(0::Int)..] (Map.elems $ p^.documents)
  return $ (Map.fromList docIdMap, Map.fromList commentIdMap)


insertDocument :: Connection -> PersistentState -> Map.Map UserName Int
               -> Int -> Document -> IO [(OverviewCommentId, Int)]
insertDocument conn p userIdMap docSqlId d@Document{..} = do
  let uploaderId = flip Map.lookup userIdMap <$> docUploader
  execute' conn "insert into documents (documentID,title,docUploader,docClass,uploadTime,docSourceURL) values (?,?,?,?,?,?)"
    (docSqlId, docTitle, uploaderId, docClassName docClass, docPostTime, docLink)
  commentMapping <- mapM (insertComment conn docSqlId userIdMap) (Map.toList docOComments)
  insertDiscussion conn docSqlId Nothing userIdMap docDiscussion
  return commentMapping

------------------------------------------------------------------------------
insertComment :: Connection -> Int -> Map.Map UserName Int
              -> (OverviewCommentId,OverviewComment)
              -> IO (OverviewCommentId,Int)
insertComment conn docSqlId userIdMap (ocId,OverviewComment{..}) = do
  allComments <- query_' conn "SELECT (commentid) FROM comments" :: IO [Only Int]
  let commentSqlId = length allComments + 1
      rating = case ocVote of
        Just (_, UpVote)   ->  1 :: Int
        Just (_, DownVote) -> -1
        Nothing            ->  0
  execute' conn
    "insert into comments (commentID,commentTime,parentDoc,parentComment,commentText) values (?,?,?,?,?)"
    (commentSqlId, ocPostTime, docSqlId, Nothing :: Maybe Int, ocText)

  execute' conn
    "insert into commentparts (commentPartID,wholeCommentID,ratingOfPaper,partIndex,text,html) values (?,?,?,?,?,?)"
    (commentSqlId, commentSqlId, rating, 0 :: Int, ocText, Nothing :: Maybe T.Text)
  let (authorQuery,userId) = case (ocPoster :: Maybe UserName) of
        Nothing    -> ("insert into anonCommentAuthors (commentID,authorID) values (?,?)",Nothing)
        Just uName -> case Map.lookup uName userIdMap of
          Nothing    -> error "Unknown user when inserting comment"
          Just uId   -> ("insert into publicCommentAuthors (commentID,authorID) values (?,?)",Just uId)
  execute' conn authorQuery
    (commentSqlId,userId)

  insertDiscussion conn docSqlId (Just commentSqlId) userIdMap ocDiscussion

  return (ocId,commentSqlId)

------------------------------------------------------------------------------
insertCommentVotes :: Connection -> PersistentState
                   -> Map.Map OverviewCommentId Int
                   -> Map.Map UserName Int -> IO ()
insertCommentVotes conn p commentIdMap userIdMap =
  forM_ (Map.elems $ p^.users) $ \u ->
  forM_ (userHistory u) $ \case
    VotedOnOComment _ cId (Just voteDir) t ->
        case (Map.lookup cId commentIdMap, Map.lookup (userName u) userIdMap) of
          (Just commentSqlId, Just userSqlId) ->
            execute conn "insert into publicCommentVotes (voterID, voteSubject, voteValue, voteTime) values (?,?,?,?)"
            (userSqlId, commentSqlId, if voteDir == UpVote then 1 else (-1 :: Int), t)
          _ -> print (unwords ["Failed to find doc/comment/user id when inserting vote"
                              , "ocId:", show cId
                              , "userId:", show (userName u)]) >> 
               return 0
    _ -> return 0

insertDiscussion :: Connection -> Int -> Maybe Int -> Map.Map UserName Int -> Discussion -> IO ()
insertDiscussion conn docSqlId parentSqlId userIdMap discussion =
  forM_ (discussion :: Forest DiscussionPoint)
  (\d -> insertDiscussionPoint conn docSqlId parentSqlId (rootLabel d) (subForest d) userIdMap)

insertDiscussionPoint :: Connection -> Int -> Maybe Int -> DiscussionPoint
                      -> Discussion -> Map.Map UserName Int -> IO ()
insertDiscussionPoint conn docSqlId parentSqlId DiscussionPoint{..} subDiscussion userIdMap = do
  allComments <- query_' conn "SELECT (commentid) FROM Comments" :: IO [Only Int]
  let commentSqlId = length allComments + 1
  execute' conn
    "INSERT into Comments (commentId, commentTime, parentDoc, parentComment, commentText) values (?,?,?,?,?)"
    (commentSqlId, _dPostTime, docSqlId, parentSqlId, _dText)
  insertDiscussion conn docSqlId (Just commentSqlId) userIdMap subDiscussion
  return ()


------------------------------------------------------------------------------
insertPinboard :: Connection -> User -> Map.Map UserName Int
               -> Map.Map DocumentId Int -> IO ()
insertPinboard conn User{..} userIdMap docMap =
  forM_ (Set.toList userPinboard) $ \docId ->
  let docSqlId  = Map.lookup docId docMap
      userSqlId = Map.lookup userName userIdMap
      pinTime   = listToMaybe . catMaybes .
                  map (\h -> case h of
                             PinnedDoc pDocId t | pDocId == docId -> Just t
                             _                                    -> Nothing) $
                  userHistory
  in  case (docSqlId, userSqlId, pinTime) of
    (Just dsID, Just usID, Just pT) -> do
      execute conn
        "INSERT INTO userPinboard (userID,docID,pinTime) values (?,?,?)"
        (usID, dsID, pT)
    _ -> error $ "Couldn't find pinboard document for "
                       ++ T.unpack userName


------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      bs <- BS.readFile fn
      case decode bs of
        Left e -> error $ "Error decoding " ++ fn ++ ": " ++ e
        Right (p :: PersistentState) -> do
          conn <- connectPostgreSQL "dbname='postgres' user='postgres'"
          userIdMap <- insertUsers conn p
          (docIdMap,commentIdMap)  <- insertDocuments conn p userIdMap
          insertCommentVotes conn p commentIdMap userIdMap
          mapM_ (\u -> insertPinboard conn u userIdMap docIdMap) (Map.elems $ p^.users)
          putStrLn $ "Done"

    _ -> error "Usage: acidToSQL filename"


------------------------------------------------------------------------------
-- A few debugging functions
execute' :: (ToRow q, Show q) => Connection -> Query -> q -> IO ()
execute' c str q = do
  putStrLn $ "Execute: " ++ show str ++ " at " ++ show q :: IO ()
  n <- execute c str q
  putStrLn $ "Done, affected rows: " ++ show n

query' :: (ToRow q, Show q, FromRow r, Show r) => Connection -> Query -> q
       -> IO [r]
query' c str q = do
  putStrLn $ "query : " ++ show str ++ " at " ++ show q
  rs <- query c str q
  putStrLn $ "Done, got: " ++ show rs
  return rs

query_' :: (FromRow r, Show r) => Connection -> Query -> IO [r]
query_' c str = do
  putStrLn $ "query_ : " ++ show str
  rs <- query_ c str
  putStrLn $ "Done, got: " ++ show rs
  return rs
