{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Control.Lens
import qualified Data.Text               as T
import qualified Network.HTTP.Client     as C

import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.AcidState
import           Reffit.AcidTypes
import           Util.Mailgun            (SigningKey)

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _acid  :: Snaplet (Acid PersistentState)
    , _sign  :: SigningKey
    , _mgr   :: C.Manager
    , _mgKey :: T.Text
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasAcid App PersistentState where
  getAcidStore = view (acid.snapletValue)

------------------------------------------------------------------------------
type AppHandler = Handler App App


