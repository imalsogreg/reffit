{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Snap where

import Control.Error
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Core

requireParam :: MonadSnap m => BS.ByteString -> m BS.ByteString
requireParam p = do
    r <- getParam p
    case r of
        Nothing ->
            finishWith
            (setResponseStatus 412
             ("Missing Parameter '" <> p <>  "'")
             emptyResponse)
        Just v -> return v

checkedParam
    :: MonadSnap m
    => BS.ByteString
    -> ExceptT ReffitError m BS.ByteString
checkedParam p =
    noteT (RErr 412 ("Missing parameter " <> T.decodeUtf8 p)) $
    MaybeT $ getParam p

checkedAssert :: MonadSnap m => ReffitError -> Bool -> ExceptT ReffitError m ()
checkedAssert e@(RErr _ _) b = bool (hoistEither $ Left e) (return ()) b
checkedAssert ROk _          = checkedAssert (RErr 500 "Unknown error") False

data ReffitError = RErr Int T.Text
                 | ROk

instance Monoid ReffitError where
    mempty = ROk
    ROk `mappend` r   = r
    r   `mappend` _   = r


runReffitErrorT :: MonadSnap m => ExceptT ReffitError m a -> m a
runReffitErrorT =
    exceptT
    (\case
      ROk -> do
          writeText "Unknown error"
          finishWith (setResponseStatus 500 "Unknown Error" emptyResponse)
      RErr n msg -> do
          modifyResponse $ setResponseStatus n (T.encodeUtf8 msg)
          writeText msg
          r <- getResponse
          finishWith r
    )
    return

-- runErrors :: MonadSnap m => ReffitErrors -> m ()
-- runErrors (ReffitErrors es) =
--     let eMap = M.fromListWith (<>) $ fmap (second (: [])) es
--     in case M.minViewWithKey eMap of
--         Nothing -> return ()
--         Just ((i,v), _) -> finishWith
--             (setResponseStatus i
--              (BS.pack $ show v)
--              emptyResponse
--             )
