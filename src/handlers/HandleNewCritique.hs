{-# LANGUAGE OverloadedStrings #-}

module HandleNewCritique(
  newCritiqueView,
  newCritiqueForm,
  handleNewCritique
  )
where

import           Reffit.Types
import           Reffit.AcidTypes

import           Safe
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
import           Data.Text.Encoding (decodeUtf8)
import           Text.Digestive
import           Text.Digestive.Blaze.Html5
import           Heist 
import qualified Heist.Interpreted            as I
import           Application
import qualified Text.Blaze.Html5             as H
import           Text.Digestive.Snap (runForm)
import           Text.Digestive.Heist  
import qualified Data.ByteString.Char8        as BS

newCritiqueForm :: (Monad m) => User -> UpDownVote -> Form Text m Critique
newCritiqueForm formUser critValue =
  Critique
  <$> "prose"     .: check "Not a valid critique" (not . T.null) (text Nothing)
  <*> "poster"    .: choice posterOpts Nothing
  <*> "dimension" .: choice dimOpts Nothing
  <*> pure critValue
  <*> pure [] 
  where 
    posterOpts = [(Just (userName formUser), userName formUser)
                 ,(Nothing,"Anonymous")]
    dimOpts = [(Novelty,"Novelty"),(Rigor,"Rigor"),(Coolness,"Coolness")]


newCritiqueView :: View H.Html -> H.Html
newCritiqueView view = do
  label       "poster" view "Post as: "
  inputSelect "poster" view
  
  label       "dimension" view "Critique dimension: "
  inputSelect "dimension" view 
  
  errorList "prose" view
  label     "prose" view "Article Critique"
  inputText "prose" view
  
handleNewCritique :: UpDownVote -> Handler App (AuthManager App) ()  
handleNewCritique critVal = do 
  userMap   <- query QueryAllUsers
  docs      <- query QueryAllDocs
  ft        <- query QueryAllFieldTags
  pId'      <- getParam "paperid"
  authUser' <- currentUser
  case readMay . T.unpack . decodeUtf8 <$> pId' of  
    Nothing -> writeText "paperid error" --TODO proper error message
    Just (Just pId) ->
      case (Map.lookup <$> (userLogin <$> authUser') <*> pure userMap) of
        Just (Just user) -> do
          (vw,rs) <- runForm "newCritiqueForm" $ newCritiqueForm user critVal
          case rs of
            Just critique -> do
              sId <- update $ AddCritique pId critique
              --let a = sId :: SummaryId  -- TODO: Must vote for
              -- Vote for own summary     -- user's summary automatically
              redirect . BS.pack $ "/view_article/" ++ show pId
              -- return $ Just sId --TODO: This doesn't work.
            Nothing -> do 
              heistLocal (bindDigestiveSplices vw) $ render "new_critique"