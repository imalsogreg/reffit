{-# LANGUAGE OverloadedStrings #-}

module HandleNewPaper(
  documentView,
  documentForm,
  handleNewArticle
  )
where

import           Reffit.Types
import           Reffit.AcidTypes
import           Application 
import           Snap.Snaplet.AcidState (Update, Query, Acid,
                                         HasAcid (getAcidStore),
                                         makeAcidic,
                                         update,query,acidInit)
import           Snap.Core
import           Snap.Snaplet(Handler)
import           Snap.Snaplet.Heist
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
import           Text.Digestive.Snap (runForm)
import           Text.Digestive.Heist  

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
  <*> pure Map.empty
  <*> pure Map.empty
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
    
------------------------------------------------------------------------------
-- | Handles article submission
handleNewArticle :: Handler App (AuthManager App) ()
--handleNewArticle = method GET handleForm <|> method POST handleFormSubmit
handleNewArticle = handleForm 
  where
   handleForm = do
     userMap <- query QueryAllUsers 
     dc      <- query QueryAllDocClasses 
     ft      <- query QueryAllFieldTags
     authUser' <- currentUser
     case (Map.lookup <$> (userLogin <$> authUser') <*> pure userMap) of
       Nothing -> writeText "Error - authUser not in app user database"
       Just Nothing -> writeText "Error - justNothing, I'm not sure how you'd get this."
       Just (Just user)  -> do
         (vw,rs) <- runForm "new_paper_form" $ documentForm user dc ft
         case rs of 
           Just doc -> do --TODO add the actual paper, not this test paper.
             _ <- update $ AddDocument doc
             writeText . (T.append "Got Document: " ) . T.pack . show $ doc
           Nothing -> do
             heistLocal (bindDigestiveSplices vw) $ render "_new_paper"  -- TODO: which one?? 
