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
      when (not . T.null . userEmail $ u) $ 
        execute' conn [sql| INSERT INTO emailaddys
                            (emailAddy,userID,verified,isPrimary)
                            values (?,?,?,?) |]
        (userEmail u, uSqlID, False, True)
      return (userName u, uSqlID)

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
  commentIdMap <- concat <$> mapM (insertDocument conn p userIdMap )
                             (Map.elems $ p^.documents)
  return $ (Map.fromList docIdMap, Map.fromList commentIdMap)


insertDocument :: Connection -> PersistentState -> Map.Map UserName Int
               -> Document -> IO [(OverviewCommentId, Int)]
insertDocument conn _ userIDMap Document{..} = do
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
    
  commentMapping <- mapM (insertComment conn docSqlID userIDMap)
                    (Map.toList docOComments)
  forM_ docAuthors $ \dAuthor -> do
    let (given:surs) = T.words dAuthor
    [Only authorSqlID] <- query' conn
          [sql| INSERT INTO authors
                (authorGivenName, authorSurname)
                VALUES (?,?) 
                RETURNING authorID |]
          (given, T.unwords surs)
    execute' conn
          [sql| INSERT INTO documentAuthors
                VALUES (?,?) |]
          (authorSqlID :: Int,docSqlID)
  insertDiscussion conn docSqlID Nothing userIDMap docDiscussion
  return commentMapping

------------------------------------------------------------------------------
insertComment :: Connection -> Int -> Map.Map UserName Int
              -> (OverviewCommentId,OverviewComment)
              -> IO (OverviewCommentId,Int)
insertComment conn docSqlID userIdMap (ocID,OverviewComment{..}) = do

  let rating = case ocVote of
        Just (_, UpVote)   ->  1 :: Int
        Just (_, DownVote) -> -1
        Nothing            ->  0
  [Only (cSqlID :: Int)] <- query' conn
    [sql| INSERT INTO comments
          (commentTime,parentDoc,commentText)
          VALUES (?,?,?) 
          RETURNING commentID |]
    (ocPostTime, docSqlID, ocText)

  zipWithM_  (\i p ->
    execute' conn
      [sql| INSERT INTO commentParts
            (wholeCommentID, commentRating, partIndex, text)
            VALUES (?,?,?,?) |]
      (cSqlID, rating, i :: Int, ocText))
    [1..] (T.lines ocText) 
  
  let (authorQuery,userID) = case (ocPoster :: Maybe UserName) of
        Nothing    ->
          ([sql| INSERT INTO anonCommentAuthors (commentID,authorID)
                 VALUES (?,?) |],Nothing)
        Just uName -> case Map.lookup uName userIdMap of
          Nothing    -> error "Unknown user when inserting comment"
          Just uId   -> ( [sql| insert into publicCommentAuthors
                                (commentID,authorID)
                                values (?,?) |],Just uId)
  execute' conn authorQuery
    (cSqlID,userID)

  when (rating /= 0 && isJust ocPoster) $
    execute' conn 
     [sql| INSERT INTO publicVotes 
           (voterID, voteDocument, voteValue, votetime)
           VALUES (?,?,?,?) |] 
     (userID, docSqlID, rating, ocPostTime)

  when (rating /= 0 && ocPoster == Nothing) $
    error "There should be no anonymous votes in the acidstate db!"

  insertDiscussion conn docSqlID (Just cSqlID) userIdMap ocDiscussion

  return (ocID,cSqlID)


------------------------------------------------------------------------------
insertCommentVotes :: Connection -> PersistentState
                   -> Map.Map OverviewCommentId Int
                   -> Map.Map UserName Int -> IO ()
insertCommentVotes conn p commentIdMap userIdMap =
  forM_ (Map.elems $ p^.users) $ \u ->
  forM_ (userHistory u) $ \case
    VotedOnOComment _ cId (Just voteDir) t ->
        case (Map.lookup cId commentIdMap, 
              Map.lookup (userName u) userIdMap) of
          (Just commentSqlId, Just userSqlId) ->
            execute conn 
              [sql| insert into publicVotes 
                    (voterID, voteComment, voteValue, voteTime) 
                    values (?,?,?,?) |]
            (userSqlId, commentSqlId, if voteDir == UpVote 
                                      then 1 else (-1 :: Int), t)
          _ -> do 
            print $
             unwords ["Failed to find doc/comment/user id when inserting vote"
                     , "ocId:", show cId
                     , "userId:", show (userName u)]
            return 0
    _ -> return 0


------------------------------------------------------------------------------
insertDiscussion :: Connection -> Int -> Maybe Int -> Map.Map UserName Int 
                 -> Discussion -> IO ()
insertDiscussion conn docSqlId parentSqlId userIdMap discussion =
  forM_ (discussion :: Forest DiscussionPoint) $ \d ->
  insertDiscussionPoint conn docSqlId parentSqlId
    (rootLabel d) (subForest d) userIdMap

insertDiscussionPoint :: Connection -> Int -> Maybe Int -> DiscussionPoint
                      -> Discussion -> Map.Map UserName Int -> IO ()
insertDiscussionPoint
  conn docSqlID parentSqlID DiscussionPoint{..} subDiscussion userIdMap = do

  [Only commentSqlID] <- query' conn
    [sql| INSERT INTO comments
          (commentTime, parentDoc, commentText)
          values (?,?,?)
          RETURNING commentID |]
    (_dPostTime, docSqlID, _dText)

  zipWithM_  (\i t ->
               execute' conn
               [sql| INSERT INTO commentParts
                     (wholeCommentID,commentRating,partIndex,parentCommentPart)
                     VALUES (?,?,?,?) |]
               (commentSqlID, commentPartRating _dText, i :: Int, parentSqlID))
    [0..] (T.lines _dText)
    
  insertDiscussion conn docSqlID (Just commentSqlID) userIdMap subDiscussion
  return ()


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

{-
insertFieldTags :: Connection -> FieldTags -> TagPath -> IO ()
insertFieldTags ts accPath = mapM_ (insertFieldTag accPath) ts


insertFieldTag :: Connection -> TagPath -> FieldTag -> IO ()
insertFieldTag conn accPath (Node tag subTags) = do
  execute' conn "INSERT INTO hashTags VALUES (?,?,?,?)"
    (showPath accPath, tag, parentSqlID, 
-}


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
          conn <- connectPostgreSQL "dbname='reffit' user='greghale'"
          userIdMap <- insertUsers conn p
          (docIdMap,commentIdMap)  <- insertDocuments conn p userIdMap
          insertCommentVotes conn p commentIdMap userIdMap
          mapM_ (\u -> insertPinboard conn u userIdMap docIdMap)
            (Map.elems $ p^.users)
          putStrLn $ "Done"

    _ -> error "Usage: acidToSQL filename"


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
