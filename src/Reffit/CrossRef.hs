{-# LANGUAGE OverloadedStrings #-}

module Reffit.CrossRef where

import Reffit.Types
import Reffit.Document

import qualified Network.HTTP as H
import Network.URI
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Lens
import qualified Data.Aeson as A
import Control.Lens.Aeson

apiUrl :: T.Text
apiUrl = "http://search.crossref.org/dois?q="
--apiUrl = "http://dx.doi.org/"

jsonFromDOI :: String -> IO BS.ByteString
jsonFromDOI doi = do
  (_,rsp) <- Network.Browser.browse $ do
    setAllowRedirects True
    let uriA = URIAuth "" "search.crossref.org" ""
    let u = URI "http:" (Just uriA) "/dois" "?q=" doi
--    let r  = H.getRequest $ T.unpack apiUrl ++ doi
    let r = H.defaultGETRequest_ u
    request r
  return $ H.rspBody rsp

docHints :: String -> IO DocumentHints
docHints doiStr = do
  jStringBS <- jsonFromDOI doiStr
  let jString = T.unpack $ decodeUtf8 jStringBS
  let fullCite' = jString ^? nth 0 . key "fullCitation" . _String :: Maybe T.Text
      asAndYear' = authorsAndYearFromFullCite <$> fullCite'
      (authors,year) = maybe ([],Nothing) id asAndYear'
  return $ DocumentHints
    (maybe "" id (jString ^? nth 0 . key "title" . _String))
    authors
    (maybe "" id (jString ^? nth 0 . key "doi" . _String))
    year

data DocumentHints = DocumentHints { titleHint   :: T.Text
                                   , authorsHint :: [T.Text]
                                   , linkHint    :: T.Text
                                   , yearHint    :: Maybe Int
                                   } deriving (Show)

--authorsAndYearFromFullCite :: Maybe String -> ([T.Text], Maybe Int)
authorsAndYearFromFullCite :: T.Text -> ([T.Text],Maybe Int)
--authorsAndYearFromFullCite Nothing = ([],Nothing)
--authorsAndYearFromFullCite (Just str) =
authorsAndYearFromFullCite str =
  let tokens   = L.filter ((>1) . T.length) . map T.strip . T.splitOn "," $ str
      yearInd' = L.findIndex (T.all C.isDigit) tokens
      authors' = (\ind -> [tokens !! i | i <- [0..ind-1]]) <$> yearInd'
      year'    = listToMaybe . catMaybes . map (readMay . T.unpack) $ tokens
  in (maybe [] id authors', year')
