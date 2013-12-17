{-# LANGUAGE OverloadedStrings #-}

module HandleNewPaper(
  documentView,
  documentForm,
  handleNewArticle
  )
where

import           Reffit.Types
import           Reffit.AcidTypes
import           Reffit.FieldTag
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
import qualified Data.ByteString.Char8        as BS
import           Data.Monoid
import           Data.Hashable
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
import           GHC.Int
import qualified Data.Tree                    as RT

documentForm :: (Monad m) => User -> [DocClass] -> FieldTags -> Form Text m Document
documentForm fromUser allDocClasses allFieldTags =
  Document
  <$> "poster"   .: choice posterOpts Nothing
  <*> pure 0
  <*> "title"    .: check "Not a valid title" (not . T.null) (text Nothing)
  <*> "authors"  .: validate validateAuthors (text Nothing)
  <*> "link"     .: check "Not a valid link" (not . T.null) (text Nothing)
  <*> "docClass" .: choice classOpts Nothing
  <*> "docTags"  .: validate (validateTags allFieldTags) (text Nothing) 
  <*> pure Map.empty
  <*> pure Map.empty
    where
      posterOpts = [(Just (userName fromUser),userName fromUser)
                   ,(Nothing,"Anonymous")]  
      classOpts = [(dc,docClassName dc) | dc <- allDocClasses]

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
 
validateTags :: FieldTags -> Text -> Result Text [TagPath]
validateTags allTags formTags
  | all (\tp -> tagPathIsElem tp  allTags) allPaths =
    Success $ allPaths
  | otherwise =
    Error $ T.concat ["Unrecognized tags: " ,
                      (T.unwords . map (T.pack . showPath) . 
                       filter (\tp -> not $ tagPathIsElem tp allTags) 
                       $ allPaths)    
                     ] 
  where
    allPathStrs = map T.strip . T.splitOn "," $ formTags
    allPaths = map (T.splitOn ".") allPathStrs 
    
------------------------------------------------------------------------------
-- | Handles article submission
handleNewArticle :: Handler App (AuthManager App) ()
handleNewArticle = handleForm 
  where
   handleForm = do
     userMap <- query QueryAllUsers
     docs    <- query QueryAllDocs
     dc      <- query QueryAllDocClasses 
     ft      <- query QueryAllFieldTags
     authUser' <- currentUser
     case (Map.lookup <$> (userLogin <$> authUser') <*> pure userMap) of
       Nothing -> writeText "Error - authUser not in app user database"
       Just Nothing -> writeText "Error - justNothing, I'm not sure how you'd get this."  -- TODO Send to please-login page
       Just (Just user)  -> do 
         (vw,rs) <- runForm "new_paper_form" $ documentForm user dc ft
         case rs of 
           Just doc -> do
             let doc' = doc {docId = newId}
             _ <- update $ AddDocument doc'
             redirect . BS.pack $ "view_article/" ++ (show . docId $ doc')
             where
               newId = head . filter (\k -> Map.notMember k docs)
                       $ (tHash: tLen: tNotTaken)
               tHash = fromIntegral . hash . docTitle $ doc:: Int32  
               tLen  = fromIntegral (Map.size docs)  :: Int32
               tNotTaken = [0..maxBound] :: [Int32]
     
           Nothing -> do
             --heistLocal (bindDigestiveSplices vw) $ render "_new_paper"
             heistLocal (bindDigestiveSplices vw)
               $ renderWithSplices "_new_paper" ftSplices
               where ftSplices = do
                       "fieldTags" ## I.textSplice $ T.intercalate " | "  tagList 
                     tagList = concat $ map RT.flatten testTags :: [FieldTag]  