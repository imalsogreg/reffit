{-# LANGUAGE OverloadedStrings #-}

module Reffit.Handlers.HandleUpdateProfile(
  handleUpdateProfile
  )
where

------------------------------------------------------------------------------
import Safe
import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import qualified Data.Map as Map
import Application
import qualified Data.ByteString.Char8 as BS
import Data.Either (rights)
import qualified Data.Set as Set
import Control.Monad.Trans (liftIO)
------------------------------------------------------------------------------
import Control.Lens
import Snap.Core
import Snap.Util.FileUploads (handleFileUploads, defaultUploadPolicy, allowWithMaximumSize)
import Snap.Snaplet (Handler)
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.AcidState (update,query)
import Heist
import qualified Heist.Interpreted as I
import Text.Digestive
import Text.Digestive.Snap (runForm)
import Text.Digestive.Heist
------------------------------------------------------------------------------
import Reffit.Types
import Reffit.AcidTypes
import Reffit.User


------------------------------------------------------------------------------
updateProfileForm :: (Monad m) => User -> Form T.Text m (User,Maybe FilePath)
updateProfileForm u = (\u p -> (u,p)) <$>
  (User
       <$> pure (u^.userName)
       <*> "realName" .: validate validateRealName (text Nothing)
       <*> "email"    .: validate validateEmail    (text Nothing)
       <*> pure (u^.userFollowing)
       <*> pure (u^.userFollowedBy)
       <*> pure (u^.userPinboard)
       <*> pure (u^.userHistory)
       <*> pure (u^.userTags)
       <*> "website"        .: validate validateWebsite (text (Nothing))
       <*> "userInfoText"   .: validate validateInfo    (text (Nothing))
       <*> pure (u ^. userJoinTime)) <*>
  ("profilePicFile" .: file)
--  let a = fp :: FilePath
--  return (u,fp)


{-
  do
   picRawFile   <- "profilePicFile" .: validate validatePicFile
                                       (text (Just "default.png"))
   website      <- "website"        .: validate validateWebsiteText
                                       (text (Just ""))
   userInfoText <- "userInfoText"   .: validate validateUserInfoText
                                       (text (Just ""))
   undefined -- TODO
-}

validateRealName :: T.Text -> Result T.Text (T.Text)
validateRealName = Success -- TODO

validateEmail :: T.Text -> Result T.Text (T.Text)
validateEmail = Success -- TODO

------------------------------------------------------------------------------
validatePicFile :: T.Text -> Result T.Text (T.Text)
validatePicFile = Success -- TODO


------------------------------------------------------------------------------
validateWebsite :: T.Text -> Result T.Text (T.Text)
validateWebsite = Success -- TODO


------------------------------------------------------------------------------
validateInfo :: T.Text -> Result T.Text (T.Text)
validateInfo = Success -- TODO

------------------------------------------------------------------------------
handleUpdateProfile :: Handler App (AuthManager App) ()
handleUpdateProfile = do
  userMap   <- query QueryAllUsers
  authUser' <- currentUser
  case join $ (Map.lookup <$> (userLogin <$> authUser') <*> pure userMap) of
    Nothing -> writeText $ "handleUpdateProfile couldn't finde user in db"
    Just u -> do
      (vw,rs) <- runForm "updateProfile" $ updateProfileForm u
      case rs of
        Just (u',maybePath) -> do
          -- TODO: upload photo somewhere. Render personal info from md
          _ <- update $ UpdateProfile u'
          liftIO $ print maybePath
          handleFileUploads "tmp" defaultUploadPolicy
            (\_ -> allowWithMaximumSize 100000) $ \xs -> do
                let a = rights (map snd xs) :: [FilePath]
                writeBS . BS.pack $ Prelude.concat a
        Nothing -> do
          heistLocal (bindDigestiveSplices vw) $
            renderWithSplices "_edit_profile" (updateProfileSplices u)


------------------------------------------------------------------------------
updateProfileSplices :: Monad m => User -> Splices (I.Splice m)
updateProfileSplices u = do
  "userName"            ## I.textSplice $ u ^. userName
  "userPicPath"         ## I.textSplice $ T.append profilePicsDir
                                                   (u ^. userName)
  "realName"            ## I.textSplice $ u ^. userRealName
  "website"             ## I.textSplice $ u ^. userWebsite
  "personalDescription" ## I.textSplice $ u ^. userDescriptionText --TODO html


------------------------------------------------------------------------------
profilePicsDir :: T.Text
profilePicsDir = "profilePics/"
