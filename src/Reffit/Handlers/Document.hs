{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Reffit.Handlers.Document where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Error
import qualified Data.Text as T
import           Data.Time
------------------------------------------------------------------------------
import           Database.PostgreSQL.Simple.SqlQQ
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.PostgresqlSimple
------------------------------------------------------------------------------
import           Application      (App)
import           Reffit.DocAuthor
import           Reffit.Document
import           Reffit.Types
import           Reffit.HashTag


------------------------------------------------------------------------------
getDocOverview :: Int -> Handler App App (Maybe DocOverview)
getDocOverview = undefined

------------------------------------------------------------------------------
getDocUploaderByDocID :: Int -> Handler App App (Maybe UserName)
getDocUploaderByDocID docID = do
  res <- query [sql| SELECT userName FROM reffitUsers
                     INNER JOIN documents
                     ON documents.docUploader = reffitUsers.userID
                     WHERE  documentID = (?) |]
         (Only docID)
  case res of
    [Only uID] -> return uID
    _          -> return Nothing
  

------------------------------------------------------------------------------
getAuthorByID :: Int -> Handler App App (Maybe DocAuthor)
getAuthorByID authorID = do
  v <- listToMaybe <$> query
    "SELECT (firstname,surname) FROM authorNames WHERE authorID == (?)"
    (Only authorID)
  refID <- (listToMaybe . map fromOnly) <$> query
    "SELECT (authorReffitID) FROM authors WHERE authorID == (?)"
    (Only authorID)
  return $ maybe Nothing (\(g,s) -> Just $ DocAuthor g s refID) v


------------------------------------------------------------------------------
getAuthorsByDocID ::Int -> Handler App App [DocAuthor]
getAuthorsByDocID docID =
  query [sql| SELECT (authorGivenName, authorSurname, reffitID)
              FROM documentAuthors
              WHERE authorID = (?) |]
    (Only docID)
