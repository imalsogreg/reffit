{-# LANGUAGE OverloadedStrings #-}

module HandleNewPaper
       (handleNewPaper)
       where

import           Reffit.Types
import           Application 

import           Snap.Snaplet(Handler)
import           Control.Applicative
import           Data.Monoid   
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.Digestive
import           Heist 
import qualified Heist.Interpreted             as I
import           Application


handleNewPaper :: Handler App App ()
handleNewPaper = undefined

documentForm :: Maybe User -> [DocClass] -> Form Text m Document
documentForm fromUser docClasses =
  Document fromUser
  <$> "title"    .: check "Not a valid title" (not T.empty) (text Nothing)
  <*> "authors"  .: check "Invalid authors"  validateAuthors (text Nothing)
  <*> "link"     .: check "Not a valid link" (not T.empty) (text Nothing)
  <*> "docClass" .: choice docClasses Nothing
  <*> "

validateAuthors :: Text -> Result Text [Text]
validateAuthors authorsText
  | T.null authorsText = Error "Authors required"
  | otherwise          = Success $ T.splitOn "," authorsText