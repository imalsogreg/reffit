{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

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
insertUsers :: Connection -> PersistentState -> IO ()
insertUsers conn p = do
  mapM_ (f conn) (Map.toList userIDs)
  insertFollowers conn (_users p) userIDs
  where
    userIDs :: Map.Map UserName Int
    userIDs =  Map.fromList $ zip 
               (map userName . Map.elems $ _users p) [(0::Int)..] 
    f c (uName,i) = do
      _ <- execute' c "insert into reffitUsers (userid,username) values (?,?)"
        (i, uName :: T.Text)
      execute' conn
        "insert into emailaddys (emailID,emailAddy,userID,verified,isPrimary) values (?,?,?,?,?)"
        (i, uName, i, False, True)


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
insertDocuments :: Connection -> PersistentState -> IO ()
insertDocuments conn p = do
  zipWithM_ (insertDocument conn p) [(0::Int)..] (Map.elems $ p^.documents)


insertDocument :: Connection -> PersistentState -> Int -> Document -> IO ()
insertDocument conn p docSqlId d@Document{..} = do
  execute' conn "insert into documents (documentID,title,docClass,uploadTime,docSourceURL) values (?,?,?,?,?)"
    (docSqlId, docTitle, docClassName docClass, docPostTime, docLink)
  mapM_ (insertComment conn docSqlId) (Map.elems docOComments)
  insertDiscussion conn docSqlId Nothing docDiscussion
  return ()

insertComment :: Connection -> Int -> OverviewComment -> IO ()
insertComment conn docSqlId OverviewComment{..} = do
  allComments <- query_' conn "SELECT (commentid) FROM comments" :: IO [Only Int]
  let commentSqlId = length allComments + 1
  execute' conn
    "insert into comments (commentID,commentTime,parentDoc,parentComment,commentText) values (?,?,?,?,?)"
    (commentSqlId :: Int, ocPostTime :: UTCTime, docSqlId :: Int, Nothing :: Maybe Int, ocText)

  insertDiscussion conn docSqlId (Just commentSqlId) ocDiscussion

  return ()

insertDiscussion :: Connection -> Int -> Maybe Int -> Discussion -> IO ()
insertDiscussion conn docSqlId parentSqlId discussion =
  forM_ (discussion :: Forest DiscussionPoint)
  (\d -> insertDiscussionPoint conn docSqlId parentSqlId (rootLabel d) (subForest d))

insertDiscussionPoint :: Connection -> Int -> Maybe Int -> DiscussionPoint -> Discussion -> IO ()
insertDiscussionPoint conn docSqlId parentSqlId DiscussionPoint{..} subDiscussion = do
  allComments <- query_' conn "SELECT (commentid) FROM Comments" :: IO [Only Int]
  let commentSqlId = length allComments + 1
  execute' conn
    "INSERT into Comments (commentId, commentTime, parentDoc, parentComment, commentText) values (?,?,?,?,?)"
    (commentSqlId, _dPostTime, docSqlId, parentSqlId, _dText)
  insertDiscussion conn docSqlId (Just commentSqlId) subDiscussion
  return ()


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
          insertUsers conn p
          insertDocuments conn p
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
