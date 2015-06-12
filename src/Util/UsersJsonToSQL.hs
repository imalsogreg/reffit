{-# LANGUAGE OverloadedStrings #-}

module Util.UsersJsonToSQL where

import Data.Foldable
import Snap.Snaplet.Auth
import qualified Snap.Snaplet.Auth.Backends.JsonFile as AuthJson
import qualified Snap.Snaplet.Auth.Backends.PostgresqlSimple

{-
migrateUsers :: Handler App (AuthManager App) ()
migrateUsers = do
  jsonAuth <- liftIO (mkJsonAuthMgr "users.json")
  undefined
-}
