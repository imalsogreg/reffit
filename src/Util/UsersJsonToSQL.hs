{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.UsersJsonToSQL where

import Control.Lens
import Control.Monad.State (get)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson.Lens
import qualified Data.Text as T
import Data.Foldable
import Snap
import Snap.Snaplet.Auth
import qualified Snap.Snaplet.Auth.Backends.JsonFile as AuthJson
import qualified Snap.Snaplet.Auth.Backends.PostgresqlSimple as AuthSQL

import Application

migrateUsers :: Handler App (AuthManager App) ()
migrateUsers = do
  let jsonFile = "users.json"
  jsonAuth  <- liftIO (AuthJson.mkJsonAuthMgr jsonFile)
  fileObj   <- A.decode <$> liftIO (BSL.readFile jsonFile)
  case fileObj of
    Nothing -> writeText "Couldn't decode json file. No user migration happened"
    Just (js :: A.Value) ->
      let myUsers = js ^.. key "uidCache"
                       . values
                       . _Array
                       . ix 1
                       . key "login"
                       . _String :: [T.Text]
      in if null myUsers
         then writeText "No users found. No migration done"
         else do
           res <- mapM ((userJsonToSQL jsonAuth)) (drop 0 myUsers)
           writeText . T.pack $ show res

-- userJsonToSQL :: AuthManager AuthJson.JsonFileAuthManager -> T.Text -> Handler App App (Bool)
userJsonToSQL jsonBackend uName = do
  u <- liftIO $ lookupByLogin jsonBackend uName
  maybe
    (return undefined)
    (\foundUser -> do
         --savedUser <- liftIO $ save foundUser
         --liftIO $ print savedUser
         --either (\_ -> return False) (\_ -> return True) savedUser)
         liftIO (print ("HELLO. uName: " ++ show uName))
         withBackend $ \b -> liftIO (do
                                         let unIdUser = foundUser {userId = Nothing}
                                         result <- save b unIdUser
                                         print ("Saving: " ++ show unIdUser)
                                         print ("Result: " ++ show result)
                                         return result
                                    )
    )
    u
