{-# LANGUAGE OverloadedStrings #-}

module Util.ReffitMigrate where

import Reffit.Types
import Reffit.AcidTypes

import Data.Serialize 
import System.IO
import Snap.Core
import           Snap.Snaplet(Handler)
import Snap hiding (put, get)
import           Application 
import           Snap.Snaplet.AcidState (Update, Query, Acid,
                                         HasAcid (getAcidStore),
                                         makeAcidic,
                                         update,query,acidInit)
import           Snap.Snaplet.Auth 
--import Data.Acid hiding (query)
import Snap.Snaplet.AcidState (Update, Query, Acid, query, acidInit)
import Data.SafeCopy
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Trans
 
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
 