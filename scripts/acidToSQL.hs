{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Data.Time
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Serialize
import Database.PostgreSQL.Simple
import System.Environment

import Reffit.Types
import Reffit.User
import Reffit.AcidTypes


------------------------------------------------------------------------------
insertUsers :: Connection -> PersistentState -> IO ()
insertUsers conn p = do
  mapM_ (f conn) (Map.toList userIDs)
  insertFollowers conn (_users p) userIDs
  where
    userIDs :: Map.Map UserName Int
    userIDs = Map.fromList $ zip 
              (map userName . Map.elems $ _users p) [(0::Int)..] 
    f c (uName,i) = do
      _ <- execute c "insert into reffitUsers (userid,username) values (?,?)"
        (i, uName :: T.Text)
      execute conn
        "insert into emailaddys (emailID,emailAddy,userID,verified,isPrimary) values (?,?,?,?,?)"
        (i, uName, i, False, True)


------------------------------------------------------------------------------
insertFollowers :: Connection
                -> Map.Map UserName User -> Map.Map UserName Int -> IO ()
insertFollowers conn rUsers userSqlIDs =
  forM_ (allFollowPairs) $ execute conn
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
main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      bs <- BS.readFile fn
      case decode bs of
        Left e -> error $ "Error decoding " ++ fn ++ ": " ++ e
        Right (s :: PersistentState) -> do
          conn <- connectPostgreSQL "dbname='postgres' user='postgres'"
          insertUsers conn s
          putStrLn $ "Done"
    _ -> error "Usage: acidToSQL filename"
