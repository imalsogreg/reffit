{-# LANGUAGE OverloadedStrings #-}

module Reffit.Handlers.HandleNewPaper(
  documentView,
  documentForm,
  handleNewArticle,
  tagButtonSplice
  )
where

import           Reffit.Types
import           Reffit.AcidTypes
import           Reffit.Document
import           Reffit.User
import           Reffit.Scores
import           Reffit.FieldTag
import           Reffit.CrossRef

import           Application
import           Snap.Snaplet.AcidState (update,query)
import           Snap.Core
import           Snap.Snaplet(Handler)
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Control.Applicative
import qualified Data.ByteString.Char8        as BS
import           Data.Hashable
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.Digestive
import           Text.Digestive.Blaze.Html5
import           Heist
import qualified Heist.Interpreted            as I
import qualified Text.Blaze.Html5             as H
import qualified Text.XmlHtml                 as X
import           Text.Digestive.Snap (runForm)
import           Text.Digestive.Heist
import           GHC.Int
import qualified Data.Tree                    as RT
import           Data.Time
import           Control.Monad
import           Control.Monad.Trans
import           Data.Monoid ((<>))

documentForm :: (Monad m) => User -> [DocClass] -> FieldTags -> (Maybe DocumentHints)
                -> UTCTime -> Form Text m Document
documentForm fromUser allDocClasses allFieldTags hints' t =
  Document
  <$> "poster"   .: choice posterOpts Nothing
  <*> pure 0
  <*> "title"    .: check "Not a valid title" (not . T.null) (text (Just defTitle))
  <*> "authors"  .: validate validateAuthors (text (Just defAuthors))
  <*> "link"     .: check "Not a valid link" (not . T.null) (text (Just defLink))
  <*> "docClass" .: choice classOpts Nothing
  <*> "docTags"  .: validate (validateTags allFieldTags) (text Nothing)
  <*> pure Map.empty
  <*> pure t
  <*> pure []
    where
      posterOpts = [(Just (userName fromUser),userName fromUser)
                   ,(Nothing,"Anonymous")]
      classOpts  = [(dc,docClassName dc) | dc <- allDocClasses]
--      tempTime   = UTCTime (ModifiedJulianDay 0) (fromIntegral (0::Int))
      defTitle   = maybe "" titleHint hints'
      defAuthors = maybe "" (T.intercalate ", " . authorsHint) hints'
--      defYear    = maybe "" (T.pack . show . yearHint) hints'
      defLink    = maybe "" linkHint hints'

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
     doi'    <- getParam "doi"
     t       <- liftIO $ getCurrentTime
     hints <- case doi' of
       Nothing  -> return Nothing
       Just doi -> liftIO $ Just <$> (docHints (BS.unpack doi))
     authUser' <- currentUser
     case join (Map.lookup <$> (userLogin <$> authUser') <*> pure userMap) of
       Nothing -> writeText "Error - authUser not in app user database"
       Just user  -> do
         (vw,rs) <- runForm "new_paper_form" $ documentForm user dc ft hints t
         case rs of
           Just doc -> do
             let doc' = doc {docId = newId}
                 user' = maybe Nothing (const $ Just user) (docUploader doc')
             _ <- update $ AddDocument user' doc'
             redirect . BS.pack $ "/view_article/" ++ (show . docId $ doc')
             where
               newId = head . filter (\k -> Map.notMember k docs)
                       $ (tHash: tLen: tNotTaken)
               tHash = fromIntegral . hash . docTitle $ doc:: Int32
               tLen  = fromIntegral (Map.size docs)  :: Int32
               tNotTaken = [0..maxBound] :: [Int32]

           Nothing -> do
             heistLocal (bindDigestiveSplices vw)
               $ renderWithSplices "_new_paper" (ftSplices <> repSplices)
               where ftSplices = do
                       "tagsButton" ## tagButtonSplice tagHierarchy
                     repSplices = do
                       "userRep" ## I.textSplice $ T.pack . show $ userReputation docs user
-- These splices are for the button, which should have the 'add tag label'
-- as a parent list-item, because tree.js shows the first list item on load.
tagButtonSplice :: (Monad m) => FieldTags -> I.Splice m
tagButtonSplice tags = return $ [X.Element "ul"  [] $
                                 [X.Element "li" [] $
                                 [X.Element "a" [] [X.TextNode "Choose Tag"]
                                 , tagTreeButtonNode [] tags]]]

tagTreeButtonNode :: TagPath -> FieldTags -> X.Node
tagTreeButtonNode basePath tags = X.Element "ul" []
                                  $ map (fieldTagButtonNode basePath) tags

fieldTagButtonNode :: TagPath -> RT.Tree FieldTag -> X.Node
fieldTagButtonNode basePath (RT.Node n [])   =
  X.Element "li" [("name", toFullName $ basePath ++ [n])]
  [X.Element "a" [] [X.TextNode n],
   X.Element "a"
   [("name", toFullName $ basePath ++ [n]),("class","btn-add btn btn-default btn-xs"),("shortName",n)]
   [X.TextNode "Add"]
  ]
fieldTagButtonNode basePath (RT.Node n subs) =
  X.Element "li" [("name", toFullName basePath')]
  [ X.Element "a" [] [ X.TextNode $ T.append n " (..)" ]
  , X.Element "a" [("name", toFullName basePath'),("class","btn-add btn btn-default btn-xs"),("shortName",n)] [X.TextNode "Add"]
  , tagTreeButtonNode basePath' subs
  ]
  where basePath' = basePath ++ [n]
