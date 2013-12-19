{-# LANGUAGE OverloadedStrings #-}

module Reffit.CrossRef where

import qualified Network.HTTP as H 
import Network.Browser
import Safe
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Maybe (listToMaybe, catMaybes,isJust)
import Snap hiding (get)
import Application
import Snap.Core (getParam)
import Snap.Snaplet.Auth
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Aeson as A

apiUrl :: T.Text
apiUrl = "http://search.crossref.org/dois?q="
--apiUrl = "http://dx.doi.org/"

--jsonFromDOI :: String -> IO (Maybe A.Array)
jsonFromDOI :: String -> IO String 
jsonFromDOI doi = do
  (_,rsp) <- Network.Browser.browse $ do
    setAllowRedirects True
    let r  = H.getRequest $ T.unpack apiUrl ++ doi
    request r
  return $ H.rspBody rsp
--return . A.decode . BSL.pack . H.rspBody $ rsp



authorsAndYearFromFullCitation :: T.Text -> Maybe ([T.Text], Int)
authorsAndYearFromFullCitation str =
  let tokens   = L.filter (not . T.null) . map T.strip . T.splitOn "," $ str
      yearInd' = L.findIndex (T.all C.isDigit) tokens
      authors' = (\ind -> [tokens !! i | i <- [0..ind-1]]) <$> yearInd'
      year'    = listToMaybe . catMaybes . map (readMay . T.unpack) $ tokens
  in (,) <$> authors' <*> year'

-- TODO maybe has to be method GET?
handleAddPaperByDOI :: Handler App (AuthManager App) ()
handleAddPaperByDOI = do
  doi' <- getParam "doi"
  case doi' of
    Nothing  -> writeBS "Error retreiving the doi itself"
    Just doi -> writeBS "Ok"