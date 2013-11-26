{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Reffit.Server.Application where

import Control.Lens.TH
import qualified Data.ByteString.Char8 as B
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.MongoDB.Core

data App = App
             { _heist     :: Snaplet (Heist App)
             , _foo       :: Snaplet Foo
             , _bar       :: Snaplet Bar
             , _companyName :: IORef B.ByteString
             }

makeLenses ''App

appInit :: SnapletInit App App
appInit = makeSnaplet "myapp" "My example application" Nothing $ do
  hs <- nestSnaplet "heist" $ heistInit "templates"
  fs <- nestSnaplet "foo" foo $ fooInit
  bs <- nestSnaplet "" bar $ nameSnaplet "newname" $ barInit foo
  addRoutes [ ("/hello", writeText "hello world")
            , ("/fooname", with foo namePage)
            , ("/barname", with bar namePage)
            , ("/company", companyHandler)
            ]
  wrapSite (<|> heistServe)
  ref <- liftIO $ newIORef "fooCorp"
  return $ App hs fs bs ref

namePage :: Handler b v ()
namePage = do
  mname <- getSnapletName
  writeText $ fromMaybe "This shouldn't happen" mname

companyHandler :: Handler App App ()
companyHandler = method GET getter <|> method POST setter
  where
    getter = do
      nameRef <- gets _companyName
      name <- liftIO $ readIORef nameRef
      writeBS name
    setter = do
      mname <- getParam "name"
      nameRef <- gets _companyName
      liftIO $ mapbe (return ()) (writeIORef nameRef) mname
      getter

instance HasHeist App where heistLens = subSnaplet heist

main :: IO ()
main = serveSnaplet defaultConfig appInit