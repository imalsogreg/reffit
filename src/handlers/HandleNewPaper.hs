{-# LANGUAGE OverloadedStrings #-}

module HandleNewPaper
       (handleNewPaper)
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
import           Heist 
import qualified Heist.Interpreted            as I
import           Application


handleNewPaper :: Handler App App ()
handleNewPaper = undefined

documentForm :: (Monad m) => Maybe User -> [DocClass] -> [FieldTag] -> Form Text m Document
documentForm fromUser allDocClasses allDocTags =
  Document fromUser
  <$> pure 1
  <*> "title"    .: check "Not a valid title" (not . T.null) (text Nothing)

  <*> "authors"  .: validate validateAuthors (text (Just "Abe Lincoln, Dr. Livingston"))
  <*> "link"     .: check "Not a valid link" (not . T.null) (text Nothing)
  <*> "docClass" .: choice classOpts Nothing
  <*> "docTags"  .: validate (validateTags allDocTags) (text Nothing)
  <*> pure []
  <*> pure (Map.empty) 
    where
      classOpts = [(dc,T.pack . show $ dc) | dc <- allDocClasses]
      
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