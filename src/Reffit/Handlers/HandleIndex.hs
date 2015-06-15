{-# LANGUAGE OverloadedStrings #-}

module Reffit.Handlers.HandleIndex(
  handleIndex
  )
where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Time
------------------------------------------------------------------------------
import           Heist
import qualified Heist.Interpreted as I
import           Snap                          (with)
import           Snap.Core                     (getParams, writeText)
import           Snap.Snaplet                  (Handler)
import           Snap.Snaplet.AcidState        (query)
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple hiding (query)
------------------------------------------------------------------------------
import           Application
import           Reffit.AcidTypes
import           Reffit.Document
import           Reffit.FieldTag
import           Reffit.Handlers.HandleNewPaper -- to get fieldTag button splices. TODO restructure
import           Reffit.PaperRoll               (allPaperRollSplices, paramsToStrategy)
import           Reffit.Scores
import           Reffit.Sort 
import           Reffit.Types
import           Reffit.User


------------------------------------------------------------------------------
handleIndex :: Handler App App ()
handleIndex = do
  docs        <- query QueryAllDocs
  us          <- query QueryAllUsers
  tags        <- query QueryAllFieldTags
  aUser       <- with auth currentUser
  indexParams <- getParams
  tNow        <- liftIO $ getCurrentTime
  docOs       <- docOverviews
  let user' = join $ Map.lookup <$> (userLogin <$> aUser) <*> pure us
  stats' <- usageStats
  case stats' of
    Just stats ->
      renderWithSplices "_index"
      (allIndexSplices tNow docOs user' indexParams tags stats)
    Nothing -> writeText "Error getting usage stats"

type IntDocOs = (DocOverview)
------------------------------------------------------------------------------
allIndexSplices :: UTCTime -> [IntDocOs]
                   -> Maybe User
                   -> Map.Map BS.ByteString [BS.ByteString]
                   -> FieldTags -> (Int, Int, Int, Int)
                   -> Splices (SnapletISplice App)
allIndexSplices tNow docs user' indexParams tags nUsers = do
  --let docsToShow = presentationSort tNow docs (paramsToStrategy tags indexParams)
  let docsToShow = docs
  allPaperRollSplices docsToShow
  allStatsSplices nUsers
  {- TODO bring back user reputation score
  case user' of
    Nothing -> do
      "tagsButton" ## tagButtonSplice tagHierarchy
    Just user -> do
      allFilterTagSplices (Set.toList . userTags $ user) <>
       ("userRep" ## I.textSplice . T.pack . show $ userReputation docs user)
  -}

------------------------------------------------------------------------------
allStatsSplices :: (Int, Int, Int, Int) -> Splices (SnapletISplice App)
allStatsSplices (nUsers, nDocs, nComments, nVotes) = do
  "nUsers"    ## I.textSplice $ T.pack . show $ nUsers
  "nDocs"     ## I.textSplice $ T.pack . show $ nDocs
  "nComments" ## I.textSplice $ T.pack . show $ nComments
  "nVotes"    ## I.textSplice $ T.pack . show $ nVotes
  where


------------------------------------------------------------------------------
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



------------------------------------------------------------------------------
usageStats :: Handler App App (Maybe (Int, Int, Int, Int))
usageStats = runMaybeT $ do
    nU  <- MaybeT $ unSql "SELECT count(*) from reffitUsers"
    nD  <- MaybeT $ unSql "SELECT count(*) from documents"
    nC  <- MaybeT $ unSql "SELECT count(*) from comments"
    nVP <- MaybeT $ unSql "SELECT count(*) from publicvotes"
    nVA <- MaybeT $ unSql "SELECT count(*) from anonvotes"
    return (nU, nD, nC, nVP + nVA)
  where
    unSql q = (listToMaybe . map fromOnly) <$> (with db $ query_ q)

------------------------------------------------------------------------------
docOverviews :: Handler App App [DocOverview]
docOverviews = query_ "SELECT * FROM documentSummary"
