{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString as BS
--import Data.Aeson
import Data.Serialize
import Database.PostgreSQL.Simple
import System.Environment

import Reffit.Types
import Reffit.AcidTypes

insertUsers :: PersistentState -> IO ()
insertUsers p = 

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      bs <- BS.readFile fn
      case decode bs of
        Left e -> error $ "Error decoding " ++ fn
        Right (s :: PersistentState) -> do
          conn <- connectPostgreSQL "dbname='postgres' user='postgres'"
          q    <- query_ conn "select 2 + 2" :: IO [Only Int]
          putStrLn $ show q
    _ -> error "Usage: acidToSQL filename"
