module Reffit.Markdown where

import           Control.Monad
import qualified Data.Text as T
import           Text.Pandoc
import qualified Text.Pandoc as Pandoc
import           Text.Pandoc.Error
import qualified Text.XmlHtml as XML
import           Data.Text.Encoding (encodeUtf8)
import           Heist.Interpreted (Splice, textSplice)

------------------------------------------------------------------------------
markdownSplice :: (Monad m) => T.Text -> Splice m
markdownSplice t =
  either
  (textSplice . T.pack)
  (return . XML.docContent)
  . XML.parseHTML "comment" . encodeUtf8 . T.pack
  . Pandoc.writeHtmlString writerOpts
  . pandocParseFallback Pandoc.def
  . T.unpack
  . T.filter (/= '\r')
  $ t

------------------------------------------------------------------------------
writerOpts :: Pandoc.WriterOptions
writerOpts = Pandoc.def { Pandoc.writerHTMLMathMethod =
                             Pandoc.MathML Nothing }

pandocParseFallback :: ReaderOptions -> String -> Pandoc
pandocParseFallback opt s =
  either (const mempty [(Para [Str s])]) id (Pandoc.readMarkdown opt s)

pandocEither :: Either PandocError Pandoc -> Pandoc
pandocEither (Right p) = p
pandocEither (Left e) = Pandoc mempty [Para [Str (show e)]]

