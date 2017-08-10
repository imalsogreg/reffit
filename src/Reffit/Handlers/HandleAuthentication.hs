{-# LANGUAGE OverloadedStrings #-}

-- Handlers for working with user authentication. Login, Logout etc.
module Reffit.Handlers.HandleAuthentication (
    handleLogin
   ,handleLoginSubmit
   ,handleLogout
   ) where

import           Application

import           Snap.Core
import           Data.Map.Syntax
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist

import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T

-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err

-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" (Just "remember")
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"

-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"
