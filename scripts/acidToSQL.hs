{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Lazy as BS
--import Data.Aeson
import Data.Serialize
import Database.PostgreSQL.Simple
import System.Environment

import Reffit.Types
import Reffit.AcidTypes



main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      bs <- BS.readFile fn
      case decode bs of
        Nothing-> error $ "Error decoding " ++ fn
        Just (s :: PersistentState) -> do
          conn <- connectPostgreSQL ""
          q    <- query_ conn "select 2 + 2" :: IO [Only Int]
          putStrLn $ show q
