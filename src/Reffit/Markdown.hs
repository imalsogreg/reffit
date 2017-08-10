module Reffit.Markdown where

import           Data.Either (either)
import qualified Data.Text as T
import qualified Text.Pandoc as Pandoc
import qualified Text.XmlHtml as XML
import           Data.Text.Encoding (encodeUtf8)
import           Heist.Interpreted (Splice, textSplice)

------------------------------------------------------------------------------
markdownSplice :: (Monad m) => T.Text -> Splice m
markdownSplice t = 
  either (textSplice . T.pack) (return . XML.docContent)
  . XML.parseHTML "comment"
  . encodeUtf8
  . T.pack
  . Pandoc.writeHtmlString writerOpts
  . (either (error "bad parse") id) --TODO
  . Pandoc.readMarkdown Pandoc.def
  . T.unpack
  . T.filter (/= '\r')
  $ t

------------------------------------------------------------------------------
writerOpts :: Pandoc.WriterOptions
writerOpts = Pandoc.def { Pandoc.writerHTMLMathMethod =
                             Pandoc.MathML Nothing }
    
