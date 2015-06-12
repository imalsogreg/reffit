{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Application
import           Data.Monoid
import           Snap.Core
import qualified Snap.Test as T
import           Snap.Snaplet.Test
import           Site (app)
import           Reffit.Handlers.Document

import Database.PostgreSQL.Simple

main = putStrLn "No tests yet"

simpleReq :: T.RequestBuilder IO ()
simpleReq  = T.get "" (mempty :: Params)

simpleEvalHandler h = evalHandler Nothing simpleReq h app

simpleTest = evalHandler Nothing simpleReq (getDocUploaderByDocID 1) app

-- For ghci tests using non-snaplet form of postgresql-simple
doC :: IO Connection
doC = connect $ ConnectInfo "localhost" 5432 "greghale" "" "reffit"
