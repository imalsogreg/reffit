module Reffit.Markdown where

import qualified Control.Monad.IO.Class as IO
import           Data.Either (either)
import qualified Data.Text as T
-- import qualified Text.Pandoc as Pandoc
import qualified System.Process as Process
import qualified Text.XmlHtml as XML
import           Data.Text.Encoding (encodeUtf8)
import           Heist.Interpreted (Splice, textSplice)


------------------------------------------------------------------------------
markdownSplice :: (IO.MonadIO m) => T.Text -> Splice m
markdownSplice t = do
  let strippedCRLF = T.filter (/= '\r') t
  pandocOutput <- IO.liftIO $
    Process.readProcess "pandoc" ["--mathml", "--from=markdown"] (T.unpack strippedCRLF)
  case XML.parseHTML "comment" . encodeUtf8 $ T.pack pandocOutput of
    Left  x -> textSplice (T.pack x)
    Right x -> return (XML.docContent x)

  -- either (textSplice . T.pack) ()

  -- either (textSplice . T.pack) (return . XML.docContent)
  -- . XML.parseHTML "comment"
  -- . encodeUtf8
  -- . T.pack
  -- . Pandoc.writeHtmlString writerOpts
  -- . (either (error "bad parse") id) --TODO
  -- . Pandoc.readMarkdown Pandoc.def
  -- . T.unpack
  -- . T.filter (/= '\r')
  -- $ t



-- ------------------------------------------------------------------------------
-- writerOpts :: Pandoc.WriterOptions
-- writerOpts = Pandoc.def { Pandoc.writerHTMLMathMethod =
--                              Pandoc.MathML Nothing }
