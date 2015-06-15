{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Reffit.Handlers.Document where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Data.Aeson.Lens
import qualified Data.Aeson as A
import           Control.Error
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import           Data.Time
import qualified Data.Vector as V
------------------------------------------------------------------------------
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.FromRow
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.PostgresqlSimple
------------------------------------------------------------------------------
import           Application      (App)
import           Reffit.DocAuthor
import           Reffit.Document
import           Reffit.Types
import           Reffit.HashTag


instance FromRow DocOverview where
  fromRow = DocOverview
            <$> field
            <*> field
            <*> field
            <*> (listFromJsonField <$> field)
            <*> (listFromJsonField <$> field)
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> (listFromJsonField <$> field)

listFromJsonField :: (A.FromJSON a, A.ToJSON a) => A.Value -> [a]
listFromJsonField = (^.. _Array . traverse . _JSON)

instance ToRow DocOverview where
  toRow DocOverview{..} =
    [toField docOID
    ,toField docOTitle
    ,toField docOUploader
    ,toField (listToJsonField docOAuthors)
    ,toField (listToJsonField docOLink)
    ,toField docOClass
    ,toField docOUploadTime
    ,toField docONComments
    ,toField docONUpvotes
    ,toField docONDownvotes
    ,toField (listToJsonField docOHashTags)
    ]

listToJsonField :: (A.ToJSON a) => [a] -> A.Value
listToJsonField xs = A.Array . V.fromList . map A.toJSON $ xs

{-
------------------------------------------------------------------------------
getDocOverview :: Int -> Handler App App (Maybe DocOverview)
getDocOverview docID = do
  uploader <- getDocUploaderByDocID docID
  runMaybeT $ do
    (title, uploadTime) <- MaybeT $ listToMaybe <$> query
                           [sql| SELECT title, uploadTime FROM documents
                                 WHERE  documentID = (?) |] (Only docID)
    authors   <- MaybeT $ Just <$> getAuthorsByDocID docID
    link      <- MaybeT $ listToMaybe <$> getDocLink docID
    nComments <- MaybeT $ getDocNComments docID
    nVotes    <- MaybeT $ getDocNVotes docID
    tags      <- MaybeT $ return $ Just []
    return $ DocOverview title uploader
      authors (T.pack link) uploadTime nComments nVotes tags


------------------------------------------------------------------------------
getDocNComments :: Int -> Handler App App (Maybe Int)
getDocNComments docID =
  (listToMaybe . map fromOnly) <$> query
  [sql| SELECT count(*) FROM commentParts
        INNER JOIN comments
        ON commentParts.parentCommentPart = comments.commentID
        WHERE comments.parentDoc = (?) |] (Only docID)


------------------------------------------------------------------------------
getDocNVotes :: Int -> Handler App App (Maybe Int)
getDocNVotes docID =
  (listToMaybe . map fromOnly) <$> query
  [sql| SELECT count(*) FROM commentParts
        INNER JOIN comments
        ON comments.commentID = commentParts.wholeCommentID
        WHERE parentDoc = (?) AND commentRating <> 0 |]
  (Only docID)


------------------------------------------------------------------------------
getDocUploaderByDocID :: Int -> Handler App App (Maybe UserName)
getDocUploaderByDocID docID = do
  res <- query [sql| SELECT userName FROM reffitUsers
                     INNER JOIN documents
                     ON documents.docUploader = reffitUsers.userID
                     WHERE  documentID = (?) |] (Only docID)
  case res of
    [Only uID] -> return uID
    _          -> return Nothing

------------------------------------------------------------------------------
getDocLink :: Int -> Handler App App [String]
getDocLink docID = map fromOnly <$>
  query [sql| SELECT docSourceURL FROM documentURLs
              WHERE  documentID = (?) |]
  (Only docID)

------------------------------------------------------------------------------
getAuthorByID :: Int -> Handler App App (Maybe DocAuthor)
getAuthorByID authorID =
  listToMaybe <$>
  query [sql| SELECT authorSurname, authorGivenName, reffitID
              FROM authors
              WHERE authorID = (?) |]
  (Only authorID)


------------------------------------------------------------------------------
getAuthorsByDocID ::Int -> Handler App App [DocAuthor]
getAuthorsByDocID docID =
  query [sql| SELECT authorSurname, authorGivenName, reffitID FROM authors
              INNER JOIN documentAuthors
              ON authors.authorID = documentAuthors.authorID
              WHERE documentAuthors.documentID = (?) |]
    (Only docID)

------------------------------------------------------------------------------
-}
