{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reffit.Handlers.HandlePasswordReset where

import           Control.Applicative
import           Control.Error
import           Control.Monad.State
import           Data.Map.Syntax
import           Data.Time
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Heist.Interpreted (textSplice)
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist

import Application
import Util.Mailgun
import Util.Snap

handleRequestReset :: Handler App App ()
handleRequestReset = method GET (render "reset_request")
                 <|> method POST sendResetMail

sendResetMail :: Handler App App ()
sendResetMail = do
    email <- requireParam "email"
    tNow  <- liftIO getCurrentTime
    let tExpire  = (3600 * 24) `addUTCTime` tNow -- Expire in 1 day
        resetReq = ResetRequest tExpire (T.decodeUtf8 email)
    m   <- gets _mgr
    sk  <- gets _sign
    mgk <- gets _mgKey
    liftIO $ sendMailgunResetRequest resetReq m sk mgk
    return ()


handleExecuteReset :: Handler App App ()
handleExecuteReset = do
    sk <- gets _sign
    tk :: T.Text <- T.decodeUtf8 <$> requireParam "token"
    method GET (newPWForm tk) <|>
        method POST (executeReset sk tk)
  where
    newPWForm tk =
        renderWithSplices "_new_password" ("token" ## textSplice tk)

    executeReset :: SigningKey -> T.Text -> Handler App App ()
    executeReset sk tk = with auth $ runReffitErrorT $ do
        tNow <- liftIO getCurrentTime
        am :: AuthManager App <- lift get
        em  <- fmap T.decodeUtf8 $ checkedParam "email"
        pw  <- fmap T.decodeUtf8 $ checkedParam "password"
        pw' <- fmap T.decodeUtf8 $ checkedParam "password2"
        checkedAssert (RErr 412 "Passwords must match") (pw == pw')

        (ResetRequest rTime tEmail) <- noteT
            (RErr 500 "Failed to decode reset token") $
            hoistMaybe $ fromSafeBlob sk (T.encodeUtf8 tk)

        checkedAssert (RErr 412 "Email must match token") (em == tEmail)
        checkedAssert (RErr 403 "Reset token expired")    (tNow < rTime)

        u <- noteT (RErr 500 "User lookup failure") $
             MaybeT $ liftIO $ lookupByEmail am (em)

        fmapLT (const $ RErr 500 "Failed to save user") $
               ExceptT $ saveUser =<< liftIO (setPassword u $ T.encodeUtf8 pw)
        writeText "Password reset successful"
