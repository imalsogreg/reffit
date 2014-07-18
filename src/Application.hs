{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Debug.Trace
import Snap                            (get)
import Snap.Snaplet                    (Handler, Snaplet, snapletValue, subSnaplet, with)
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.AcidState
import Snap.Snaplet.PostgresqlSimple
import Reffit.AcidTypes

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _acid  :: Snaplet (Acid PersistentState)
    , _db    :: Snaplet Postgres
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasAcid App PersistentState where
  getAcidStore = view (acid . snapletValue)

instance HasPostgres (Handler b App) where
  getPostgresState = trace "Postgres App handler" $ with db get

--instance HasPostgres (Handler App (AuthManager App)) where
--  getPostgresState = trace "Postgres AuthManager App Handler" $ getPostgresState

------------------------------------------------------------------------------
type AppHandler = Handler App App


