{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.ReffitMigrate where


import Data.Monoid ((<>))
import qualified Control.Monad.State as St
import Data.Maybe (catMaybes)
import Data.Serialize
import System.Random
import Data.Foldable
import qualified Data.Map as M
import Data.Traversable
import System.IO
import Snap.Core
import           Snap.Snaplet(Handler)
import Snap hiding (put, get)
import           Application
import           Snap.Snaplet.AcidState (Update, Query, Acid,
                                         HasAcid (getAcidStore),
                                         makeAcidic,
                                         update,query,acidInit,
                                         createCheckpoint)
import           Snap.Snaplet.Auth
--import Data.Acid hiding (query)
import Snap.Snaplet.AcidState (Update, Query, Acid, query, acidInit)
import Data.SafeCopy
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Control.Monad.Trans

import qualified Reffit.User as R
import Reffit.Types
import Reffit.AcidTypes

handleStateToDisk :: Handler App (AuthManager App) ()
handleStateToDisk = do
  aUser <- currentUser
  case userLogin <$> aUser of
    Just "imalsogreg" -> do
      ds      <- query QueryAllDocs
      us      <- query QueryAllUsers
      classes <- query QueryAllDocClasses
      tags    <- query QueryAllFieldTags
      liftIO . BS.writeFile "backupData.bin" .
        runPut . put $ PersistentState ds us classes tags
      writeText . T.pack . show $ PersistentState ds us classes tags
    _ -> writeText "Sorry.  Only imalsogreg can do state backup"

handleStateFromDisk :: Handler App (AuthManager App) ()
handleStateFromDisk = do
  aUser <- currentUser
  case userLogin <$> aUser of
    Just "imalsogreg" -> do
      d <- liftIO $ BS.readFile "backupData.bin"
      case runGet get d of
        Right (PersistentState ds us classes tags) -> do
          _ <- update $ UpdateAllDocs ds
          _ <- update $ UpdateAllUsers us
          _ <- update $ UpdateAllDocClasses classes
          _ <- update $ UpdateAllFieldTags tags
          writeText "Took state from file"
        Left e ->
          writeText $ T.append "Parse error! " (T.pack e)
    _ -> writeText "Sorry.  Only imalsogreg can do state restore"

handleCheckpoint :: Handler App (AuthManager App) ()
handleCheckpoint = do
  aUser <- currentUser
  case userLogin <$> aUser of
    Just "imalsogreg" -> do
      _ <- createCheckpoint
      writeText "Checkpoint made"
    _ -> writeText "Only imalsogreg can create a checkpoint"

restoreAuthUsers :: Handler App (AuthManager App) ()
restoreAuthUsers = do
    aUser <- currentUser
    mgr <- St.get
    case userLogin <$> aUser of
        Just "imalsogreg" -> do
            us <- query QueryAllUsers
            res <- forM (M.toList us) $ \(uname, u) -> do
                pw :: Integer <- liftIO $ randomRIO (100000000,100000000000)
                existingU <- liftIO $ lookupByLogin mgr (R.userName u)
                case existingU of
                    Just n  -> return $ "No need, " <> R.userName u <> " exists"
                    Nothing -> T.pack . show <$>
                               createUser (R.userName u) (BS.pack $ show pw)
            writeText $ T.unlines res
        _ -> writeText "Only the admin can do that"

{-
handleMigrateStateFromDisk :: Handler App (AuthManager App) ()
handleMigrateStateFromDisk = do
  aUser <- currentUser
  case userLogin <$> aUser of
    Just "imalsogreg" -> do
      d <- liftIO $ BS.readFile "backupData.bin"
      case runGet get d of
        Right p0 -> do
          let (PersistentState ds us classes tags) = migrate p0
          _ <- update $ UpdateAllDocs ds
          _ <- update $ UpdateAllUsers us
          _ <- update $ UpdateAllDocClasses classes
          _ <- update $ UpdateAllFieldTags tags
          writeText "Took state from file"
        Left e ->
          writeText $ T.append "Parse error! " (T.pack e)
    _ -> writeText "Sorry.  Only imalsogreg can do state restore"
-}

setAuthUserEmails :: Handler App (AuthManager App) ()
setAuthUserEmails = do
    u <- currentUser
    case userLogin <$> u of
        Just "imalsogreg" -> do
            aManager <- St.get
            us      <- query QueryAllUsers
            aus <- fmap catMaybes $ for (M.elems us) $ \(u :: R.User) -> do
                liftIO $ putStrLn ("lookup login: " ++ show (R.userName u))
                lkp <- liftIO $ lookupByLogin aManager (R.userName u)
                liftIO $ print lkp
                return $ fmap (\au -> au { userEmail = Just (R.userEmail u)}) lkp
            for_ aus saveUser
        _ -> error "Only imalsogreg can do this"
