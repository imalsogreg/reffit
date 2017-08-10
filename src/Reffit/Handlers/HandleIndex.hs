{-# LANGUAGE OverloadedStrings #-}

module Reffit.Handlers.HandleIndex(
  handleIndex
  )
where

import Reffit.Types
import Reffit.Document
import Reffit.OverviewComment
import Reffit.Sort
import Reffit.PaperRoll
import Reffit.User
import Reffit.Scores
import Reffit.AcidTypes
import Reffit.FieldTag

import Reffit.Handlers.HandleNewPaper -- to get fieldTag button splices. TODO restructure

import Control.Applicative
import Snap.Core (getParams, writeText)
import Snap.Snaplet(Handler)
import Snap.Snaplet.AcidState (query)
import Snap.Snaplet.Auth
import Snap.Snaplet.Heist
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Control.Monad.Trans
import Data.Map.Syntax
import Data.Time
import Data.Monoid ((<>))

handleIndex :: Handler App (AuthManager App) ()
--handleIndex = handlePaperRoll
handleIndex = do
  docs        <- query QueryAllDocs
  us          <- query QueryAllUsers
  tags        <- query QueryAllFieldTags
  aUser       <- currentUser
  indexParams <- getParams
  tNow        <- liftIO $ getCurrentTime
  let user' = join $ Map.lookup <$> (userLogin <$> aUser) <*> pure us :: Maybe User
  renderWithSplices "_index" (allIndexSplices tNow docs user' us indexParams tags)

allIndexSplices :: UTCTime -> Map.Map DocumentId Document
                   -> Maybe User -> Map.Map UserName User
                   -> Map.Map BS.ByteString [BS.ByteString]
                   -> FieldTags
                   -> Splices (SnapletISplice App)
allIndexSplices tNow docs user' us indexParams tags  = do
  let docsToShow = presentationSort tNow docs (paramsToStrategy tags indexParams)
  allPaperRollSplices docsToShow
  allStatsSplices docs us
  case user' of
    Nothing -> do
      "tagsButton" ## tagButtonSplice tagHierarchy
    Just user -> do
      allFilterTagSplices (Set.toList . userTags $ user) <>
       ("userRep" ## I.textSplice . T.pack . show $ userReputation docs user)


allStatsSplices :: Map.Map DocumentId Document -> Map.Map UserName User -> Splices (SnapletISplice App)
allStatsSplices docs us = do
  "nUsers"    ## I.textSplice $ T.pack . show . Map.size $ us
  "nDocs"     ## I.textSplice $ T.pack . show . Map.size $ docs
  "nComments" ## I.textSplice $ T.pack . show $
    sum (map (\d -> (Map.size . docOComments $ d)) (Map.elems docs)  )
  "nVotes"    ## I.textSplice $ T.pack . show $
    sum $ map docNVotes (Map.elems docs)
  where
    docNVotes doc = sum . map (length . ocResponse) .
                    Map.elems $ docOComments doc

allFilterTagSplices :: [TagPath] -> Splices (SnapletISplice App)
allFilterTagSplices tps = do
  "fieldTags"  ## renderFieldTags tps
  "tagsButton" ## tagButtonSplice tagHierarchy

renderFieldTags :: [TagPath] -> SnapletISplice App
renderFieldTags = I.mapSplices $ I.runChildrenWith . splicesFromFieldTag

splicesFromFieldTag :: Monad n => TagPath -> Splices (I.Splice n)
splicesFromFieldTag tp = do
  "fieldTagText"     ## I.textSplice . last $ tp
  "fieldTagFullText" ## I.textSplice . toFullName $ tp

