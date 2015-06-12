{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Reffit.Handlers.Search where

import Snap.Snaplet.PostgreSqlSimple

import Reffit.Document
import Reffit.Search
import Reffit.Types

------------------------------------------------------------------------------
searchDocs :: SearchRequest -> Handler App App [DocOverview]
searchDocs SearchRequest{..} = do
  ids <- query (q searchSortBy) ()

  where
    q New nResult nOffset sGen sTitle sAuthor sTags =
      [sql| SELECT documentID FROM documents
            ORDER BY docUploadTime
            LIMIT (?) OFFSET (?)
            WHERE (?) LIKE (?) AND
            title LIKE (?) AND
            (?)   LIKE (?) AND
            (?) LIKE (?)
          |]
    q Hot nResult nOffset sGen sTitle sAuthor sTags =
      [sql| SELECT documentID FROM documents
            ORDER BY docUploadTime
            LIMIT (?) OFFSET (?)
            WHERE (?) LIKE (?) AND
            title LIKE (?) AND
            (?)   LIKE (?) AND
            (?) LIKE (?)
          |]
      

                     
