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
--import           Reffit.FieldTag -- TODO: get rid of all FieldTag stuff
import           Reffit.HashTag
import           Reffit.Handlers.HandleNewPaper -- to get fieldTag button splices. TODO restructure
import           Reffit.PaperRoll               (allPaperRollSplices, paramsToStrategy)
import           Reffit.Scores
import           Reffit.Sort
import           Reffit.Types
import           Reffit.User


------------------------------------------------------------------------------
handleIndex :: Handler App App ()
handleIndex = do
  --docs        <- query QueryAllDocs
  us          <- query QueryAllUsers
  aUser       <- with auth currentUser
  indexParams <- getParams
  tNow        <- liftIO $ getCurrentTime
  docOs       <- docOverviews
  let user' = join $ Map.lookup <$> (userLogin <$> aUser) <*> pure us
  stats' <- usageStats
  case stats' of
    Just stats ->
      renderWithSplices "_index"
      (allIndexSplices tNow docOs user' indexParams stats)
    Nothing -> writeText "Error getting usage stats"


------------------------------------------------------------------------------
allIndexSplices :: UTCTime -> [DocOverview]
                   -> Maybe User
                   -> Map.Map BS.ByteString [BS.ByteString]
                   -> (Int, Int, Int, Int)
                   -> Splices (SnapletISplice App)
allIndexSplices tNow docs user' indexParams nUsers = do
  let docsToShow = docs
  allPaperRollSplices docsToShow
  allStatsSplices nUsers


------------------------------------------------------------------------------
allStatsSplices :: (Int, Int, Int, Int) -> Splices (SnapletISplice App)
allStatsSplices (nUsers, nDocs, nComments, nVotes) = do
  "nUsers"    ## I.textSplice $ T.pack . show $ nUsers
  "nDocs"     ## I.textSplice $ T.pack . show $ nDocs
  "nComments" ## I.textSplice $ T.pack . show $ nComments
  "nVotes"    ## I.textSplice $ T.pack . show $ nVotes
  where


------------------------------------------------------------------------------
usageStats :: Handler App App (Maybe (Int, Int, Int, Int))
usageStats = runMaybeT $ do
    nU  <- MaybeT $ unSql "SELECT count(*) from reffitUsers"
    nD  <- MaybeT $ unSql "SELECT count(*) from documents"
    nC  <- MaybeT $ unSql "SELECT count(*) from comments"
    nVP <- MaybeT $ unSql "SELECT count(*) from votes"
    return (nU, nD, nC, nVP)
  where
    unSql q = (listToMaybe . map fromOnly) <$> (with db $ query_ q)

------------------------------------------------------------------------------
docOverviews :: Handler App App [DocOverview]
docOverviews = query_ "SELECT * FROM documentSummary"
