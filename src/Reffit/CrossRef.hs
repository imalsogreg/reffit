{-# LANGUAGE OverloadedStrings #-}

module Reffit.CrossRef where

import qualified Network.HTTP as H 
import Network.Browser
import Data.Aeson
import qualified Data.Text as T 
import Snap hiding (get)
import Application
import Snap.Core (getParam)
import Snap.Snaplet.Auth
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as BS

apiUrl :: T.Text
apiUrl = "http://dx.doi.org/"


jsonFromDOI doi = do
  (_,rsp) <- browse $ do
    setAllowRedirects True
    let r  = getRequest $ T.unpack apiUrl ++ doi
        r' = H.setHeaders r
             [H.Header H.HdrAccept "application/citeproc+json"]
    request $ r'
  fmap (take 100) (H.rspBody rsp)
  
-- TODO maybe has to be method GET?
handleAddPaperByDOI :: Handler App (AuthManager App) ()
handleAddPaperByDOI = do
  doi' <- getParam "doi"
  case doi' of
    Nothing  -> writeBS "Error retreiving the doi itself"
    Just doi -> writeBS "Ok"