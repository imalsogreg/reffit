{-# LANGUAGE OverloadedStrings #-}

module HandleNewPaper(
  documentView,
  documentForm
  )
where

import           Reffit.Types
import           Application 

import           Snap.Snaplet(Handler)
import           Snap.Snaplet.Auth
import           Control.Applicative
import           Data.Monoid
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.Digestive
import           Text.Digestive.Blaze.Html5
import           Heist 
import qualified Heist.Interpreted            as I
import           Application
import qualified Text.Blaze.Html5             as H

-- This is written in Site.hs.  Maybe move to here?
--handleNewPaper :: Handler App App ()
--handleNewPaper = undefined

documentForm :: (Monad m) => User -> [DocClass] -> [FieldTag] -> Form Text m Document
documentForm fromUser allDocClasses allDocTags =
  Document
  <$> "poster"   .: choice posterOpts Nothing
  <*> pure 0
  <*> "title"    .: check "Not a valid title" (not . T.null) (text Nothing)

  <*> "authors"  .: validate validateAuthors (text (Just "Abe Lincoln, Dr. Livingston"))
  <*> "link"     .: check "Not a valid link" (not . T.null) (text Nothing)
  <*> "docClass" .: choice classOpts Nothing
  <*> "docTags"  .: validate (validateTags allDocTags) (text Nothing)
  <*> pure []
  <*> pure (Map.empty) 
    where
      posterOpts = [(Just (userName fromUser),userName fromUser)
                   ,(Nothing,"Anonymous")]  
      classOpts = [(dc,T.pack . show $ dc) | dc <- allDocClasses]

documentView :: View H.Html -> H.Html
documentView view = do

  label       "poster" view "Post as: "
  inputSelect "poster" view
  
  errorList "title" view
  label     "title" view "Title: "
  inputText "title" view

  errorList "authors" view
  label     "authors" view "Authors: "
  inputText "authors" view

  errorList "link" view
  label     "link" view "Link to paper: "
  inputText "link" view

  label       "docClass" view "Document Type: "
  inputSelect "docClass" view

  errorList "tags" view
  label     "tags" view "Field Tags: "
  inputText "tags" view
      
validateAuthors :: Text -> Result Text [Text]
validateAuthors authorsText
  | T.null authorsText = Error "Authors required"
  | otherwise          = Success $ T.splitOn "," authorsText

validateTags :: [FieldTag] -> Text -> Result Text [FieldTag]
validateTags allTags' formTags'
  | all (`elem` allTags) formTags =
    Success . map FieldTag $ formTags
  | otherwise =
    Error $ T.concat ["Unrecognized tags: " ,
    (T.unwords . filter (`notElem` allTags) $ formTags)]
  where
    allTags  = map (\(FieldTag t) -> t) allTags' 
    formTags = T.words formTags'