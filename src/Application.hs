{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.AcidState
import Reffit.AcidTypes

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _acid :: Snaplet (Acid PersistentState)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasAcid App PersistentState where
  getAcidStore = view (acid.snapletValue)

------------------------------------------------------------------------------
type AppHandler = Handler App App


