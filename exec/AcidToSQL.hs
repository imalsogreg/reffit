{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE QuasiQuotes         #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Bool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List
import Data.Monoid

import qualified Data.Map as Map
import Data.Maybe
import Data.Serialize
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time
import Data.Tree
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import GHC.Int
import System.Environment

import Reffit.Document
import Reffit.Types
import Reffit.User
import Reffit.OverviewComment
import Reffit.Discussion
import Reffit.AcidTypes
import Reffit.FieldTag


------------------------------------------------------------------------------
insertUsers :: Connection -> PersistentState -> IO (Map.Map UserName Int)
insertUsers conn p = do
  userSqlIDs <- Map.fromList <$> mapM (f conn) (Map.elems $ p^.users)
  insertFollowers conn (_users p) userSqlIDs
  return userSqlIDs
  where
    f c u = do
      [Only uSqlID] <- query' c [sql| INSERT into reffitUsers
                                      (username, userJoinTime)
                                      values (?,?)
                                      RETURNING userid |]
        (userName u :: T.Text, userJoinTime u)
      unless (T.null . userEmail $ u) $
        execute' conn [sql| INSERT INTO emailaddys
                            (emailAddy,userID,verified,isPrimary)
                            values (?,?,?,?) |]
        (userEmail u, uSqlID, False, True)
      return (userName u, uSqlID)

------------------------------------------------------------------------------
insertFollowers :: Connection
                -> Map.Map UserName User -> Map.Map UserName Int -> IO ()
insertFollowers conn rUsers userSqlIDs =
  forM_ allFollowPairs $ execute' conn
  "insert into userFollowers (follower,followed,followTime) values (?,?,?)"
  where
    allFollowPairs = concatMap userFollowed (Map.elems rUsers)

    userFollowed :: User -> [(Int,Int,UTCTime)]
    userFollowed u = mapMaybe (pairFollow u)
                     (Set.toList $ userFollowing u)

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
                -> IO (Map.Map DocumentId Int, Map.Map OverviewCommentId Int, Map.Map DiscussionPointReffitId DiscussionPointSQLId)
insertDocuments conn p userIdMap = do
  let docIdMap = zip (map fst . Map.toList $ p^.documents) [(0::Int)..]
  (commentIdMaps,discussionIdMaps) <- unzip <$> mapM (insertDocument conn p userIdMap )
                             (Map.elems $ p^.documents)
  return $ (Map.fromList docIdMap, Map.fromList (concat commentIdMaps), mconcat discussionIdMaps)


insertDocument :: Connection -> PersistentState -> Map.Map UserName Int
               -> Document -> IO ([(OverviewCommentId, Int)], Map.Map DiscussionPointReffitId DiscussionPointSQLId)
insertDocument conn _ userIDMap doc@Document{..} = do
  let uploaderId = flip Map.lookup userIDMap <$> docUploader
  [Only docSqlID] <- query' conn
    [sql| INSERT INTO documents
          (title,docUploader,docClass,uploadTime)
          VALUES (?,?,?,?)
          RETURNING documentID |]
    (docTitle, uploaderId, docClassName docClass, docPostTime)
  execute' conn
    [sql| INSERT INTO documentURLs VALUES (?,?) |]
    (docSqlID, docLink)

  (commentAcidIds, commentSQLIds, discPntMaps) <- unzip3 <$> mapM (insertComment conn docSqlID userIDMap)
                    (Map.toList docOComments)
  forM_ docAuthors $ \dAuthor -> do
    let (given:surs) = T.words dAuthor
    --[Only authorSqlID] <- query' conn
    --      [sql| INSERT INTO documentauthors
    --            (authorGivenName, authorSurname)
    --            VALUES (?,?)
    --            RETURNING authorID |]
    --      (given, T.unwords surs)
    execute' conn
          [sql| INSERT INTO documentAuthors(documentID, nameString)
                VALUES (?,?) |]
          (docSqlID :: Int, T.unwords (given:surs))
  discussionIdMap <- insertDiscussion conn docSqlID Nothing userIDMap docDiscussion Map.empty
  insertDocFieldTags conn (fromIntegral docSqlID) doc
  return (zip commentAcidIds commentSQLIds :: [(OverviewCommentId, Int)],
          mconcat (discussionIdMap:discPntMaps) :: Map.Map DiscussionPointReffitId DiscussionPointSQLId)

------------------------------------------------------------------------------
insertComment :: Connection -> Int -> Map.Map UserName Int
              -> (OverviewCommentId,OverviewComment)
              -> IO (OverviewCommentId,Int, Map.Map DiscussionPointReffitId DiscussionPointSQLId)
insertComment conn docSqlID userIdMap (ocID,OverviewComment{..}) = do

  let rating = case ocVote of
        Just (_, UpVote)   ->  1 :: Int
        Just (_, DownVote) -> -1
        Nothing            ->  0
  let commentAuthor = flip Map.lookup userIdMap =<< (ocPoster) :: Maybe Int
  [Only (cSqlID :: Int)] <- query' conn
    [sql| INSERT INTO comments
          (commentTime,userID,commentRating,parentDoc,parentComment,commentText)
          VALUES (?,?,?,?,?,?)
          RETURNING commentID |]
    (ocPostTime, commentAuthor, rating, docSqlID, (Nothing :: Maybe Int), ocText)

  when (rating /= 0 && isJust commentAuthor) $
    execute' conn
     [sql| INSERT INTO votes
           (userID, voteDocument, voteComment, voteValue, votetime)
           VALUES (?,?,?,?,?) |]
     (commentAuthor, docSqlID, (Nothing :: Maybe Int), rating, ocPostTime)

  discussionIdMap <- insertDiscussion conn docSqlID (Just cSqlID) userIdMap ocDiscussion Map.empty

  return (ocID,cSqlID,discussionIdMap)


------------------------------------------------------------------------------
type DiscussionPointReffitId = Int32
type DiscussionPointSQLId    = Int32

insertCommentVotes :: Connection -> PersistentState
                   -> Map.Map DocumentId Int
                   -> Map.Map OverviewCommentId Int
                   -> Map.Map DiscussionPointReffitId DiscussionPointSQLId
                   -> Map.Map UserName Int -> IO ()
insertCommentVotes conn p docIdMap commentIdMap discussionIdMap userIdMap =
  forM_ (Map.elems $ p^.users) $ \u ->
  forM_ (userHistory u) $ \case
    VotedOnOComment dId cId (Just voteDir) t ->
        case (Map.lookup dId docIdMap,
              Map.lookup cId commentIdMap,
              Map.lookup (userName u) userIdMap) of
          (Just docSqlId, Just commentSqlId, Just userSqlId) ->
            execute conn
              [sql| insert into votes
                    (userID, voteDocument, voteComment, voteValue, voteTime)
                    values (?,?,?,?,?) |]
            (userSqlId, docSqlId, commentSqlId, bool (-1 :: Int) 1 (voteDir == UpVote), t)
          _ -> do
            print $
             unwords ["Failed to find doc/comment/user id when inserting vote"
                     , "ocId:", show cId
                     , "userId:", show (userName u)]
            return 0
    _ -> return 0

------------------------------------------------------------------------------
insertDiscussion :: Connection -> Int -> Maybe Int -> Map.Map UserName Int
                 -> Discussion -> Map.Map DiscussionPointReffitId DiscussionPointSQLId
                 -> IO (Map.Map DiscussionPointReffitId DiscussionPointSQLId)
insertDiscussion conn docSqlId parentSqlId userIdMap discussion idAcc = do
  ids <- forM (discussion :: Forest DiscussionPoint) $ \d ->
    insertDiscussionPoint conn docSqlId parentSqlId
    (rootLabel d) (subForest d) userIdMap idAcc
  return (mconcat ids)

insertDiscussionPoint :: Connection -> Int -> Maybe Int -> DiscussionPoint
                      -> Discussion -> Map.Map UserName Int
                      -> Map.Map DiscussionPointReffitId DiscussionPointSQLId
                      -> IO (Map.Map DiscussionPointReffitId DiscussionPointSQLId)
insertDiscussionPoint
  conn docSqlID parentSqlID DiscussionPoint{..} subDiscussion userIdMap idAcc = do

  [Only mcommentSqlID] <- query' conn
    [sql| INSERT INTO comments
          (commentTime, parentDoc, commentText)
          values (?,?,?)
          RETURNING commentID |]
    (_dPostTime, docSqlID, _dText)

  let commSqlID = maybe (error "No comment sql id!") id mcommentSqlID :: Int
  subIDs <- insertDiscussion conn docSqlID mcommentSqlID userIdMap subDiscussion idAcc
  return (Map.singleton _dID (fromIntegral commSqlID) <> subIDs)


------------------------------------------------------------------------------
commentPartRating :: T.Text -> Int
commentPartRating p
  | "(+1)" `T.isInfixOf` p =  1
  | "(-1)" `T.isInfixOf` p = -1
  | otherwise              =  0


------------------------------------------------------------------------------
insertPinboard :: Connection -> User -> Map.Map UserName Int
               -> Map.Map DocumentId Int -> IO ()
insertPinboard conn User{..} userIdMap docMap =
  forM_ (Set.toList userPinboard) $ \docId ->
  let docSqlId  = Map.lookup docId docMap
      userSqlId = Map.lookup userName userIdMap
      justPin h = case h of
        PinnedDoc pDocId t | pDocId == docId -> Just t
        _                                    -> Nothing
      pinTime   = listToMaybe . catMaybes $
                  map justPin userHistory
  in  case (docSqlId, userSqlId, pinTime) of
    (Just dsID, Just usID, Just pT) -> do
      execute conn
        "INSERT INTO userPinboard (userID,docID,pinTime) values (?,?,?)"
        (usID, dsID, pT)
      return ()
    _ -> putStrLn $ "Couldn't find pinboard document for "
                       ++ T.unpack userName

-- This doesn't get called from anywhere in AcidToSql.
-- That is ok. In the old model, 'tags' are associated with a document directly
-- In the new model, tags will only come from hashtags in comments.
--insertFieldTags :: Connection -> FieldTags -> TagPath -> IO ()
--insertFieldTags conn ts accPath =
--  mapM_ (insertFieldTag conn accPath) (subForest ts)
--insertFieldTag :: Connection -> TagPath -> FieldTag -> IO ()
--insertFieldTag conn accPath (Node tag subTags) = do
--  execute' conn "INSERT INTO hashTags VALUES (?)" (Only tag)
--  insertFieldTags conn subTags

insertDocFieldTags :: Connection -> DocumentId -> Document -> IO ()
insertDocFieldTags conn dID Document{..} =
  forM_ docFieldTags $
  \fTag -> forM_ fTag $
           \t -> (execute' conn
                 "INSERT INTO hashTags(hashtag,docid) VALUES (?,?)"
                 (t, dID))

------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn, pw] -> do
      bs <- BS.readFile fn
      case decode bs of
        Left e -> error $ "Error decoding " ++ fn ++ ": " ++ e
        Right (p :: PersistentState) -> do
          conn <- connectPostgreSQL
                  ("dbname='reffit' user='reffit' host='localhost' password='"
                   <> BSC.pack pw <> "'")
          userIdMap <- insertUsers conn p
          (docIdMap,commentIdMap,discussionIdMap)  <- insertDocuments conn p userIdMap
          insertCommentVotes conn p docIdMap commentIdMap discussionIdMap userIdMap
          mapM_ (\u -> insertPinboard conn u userIdMap docIdMap)
            (Map.elems $ p^.users)
          putStrLn "Done"

    _ -> error "Usage: acidToSQL filename password"


------------------------------------------------------------------------------
-- A few debugging functions
execute' :: (ToRow q, Show q) => Connection -> Query -> q -> IO ()
execute' c str q = do
  putStrLn $ "Execute: " ++ show str ++ " at " ++ show q :: IO ()
  n <- execute c str q
  putStrLn $ "Done, affected rows: " ++ show n

{-
execute_' :: Connection -> Query -> IO ()
execute_' c str = do
  putStrLn "Execute_: " ++ show str :: IO ()
  n <- execute_ c str
  putStrLn $ "Done, affected rows: " ++ show n
-}

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
