{-# LANGUAGE OverloadedStrings #-}

module PaperRoll where

import Reffit.Types
import Reffit.AcidTypes
import Reffit.Document
import Reffit.Scores
import Reffit.Sort
import Reffit.Search
import Reffit.FieldTag

import Control.Applicative
import qualified Data.List as L
import Data.Maybe (listToMaybe)
import Snap.Snaplet (Handler)
import Snap.Core
import Snap.Snaplet.AcidState (query)
import Snap.Snaplet.Heist
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Control.Monad (join)
import Control.Monad.Trans (liftIO)
import Data.Time

handlePaperRoll :: Handler App App ()
handlePaperRoll = method GET handleNotGet
  where  
    handleNotGet = 
      do
        docs      <- query QueryAllDocs
        tags      <- query QueryAllFieldTags
        tNow      <- liftIO $ getCurrentTime
        indexParams <- getParams
        let docsToShow = presentationSort tNow docs 
                         (paramsToStrategy tags indexParams) 
--        renderWithSplices "paper_roll" (allPaperRollSplices docsToShow)
        writeText . T.pack . show $ paramsToStrategy tags indexParams

-- TODO - move this to Reffit.Sort?  Reffit.Search?
presentationSort :: UTCTime -> Map.Map DocumentId Document 
                    -> PresentationStrategy
                    -> [Document]
presentationSort _ docMap (SearchBy searchTerm) =
  searchDocs 10 docMap searchTerm
presentationSort tNow docMap (FiltSort s [])  =
  (sortDocs (sortF tNow s) True) . Map.elems $ docMap
presentationSort tNow docMap (FiltSort s fts) = 
  (sortDocs (sortF tNow s) True) 
  . filter (\d -> any (\dtag -> any (tagIncludes dtag) fts) (docFieldTags d)) 
  . Map.elems $ docMap

sortF :: UTCTime -> SortBy -> (Document -> Int)
sortF t s = case s of
  New -> floor . (\d -> diffUTCTime (docPostTime d) t0)
  Hot -> hotnessScore t
  Popular -> qualityScore
  Controversial -> controversyScore
      
t0 :: UTCTime
t0 = UTCTime (ModifiedJulianDay 0) 0

-- TODO pagination.  add Int and Int to each constructor for startInd and #of
data PresentationStrategy = FiltSort SortBy [TagPath]
                          | SearchBy T.Text
                          deriving (Show, Eq)

paramsToStrategy :: FieldTags -> Map.Map BS.ByteString [BS.ByteString] 
                    -> PresentationStrategy
paramsToStrategy tags params = case Map.lookup "q" params of
  Just (searchTerms:_) -> SearchBy . decodeUtf8 $ searchTerms
  Just []              -> FiltSort New []
  Nothing -> 
    let sortCrit = maybe New id $ do 
          sortStrs <- Map.lookup "sortBy" params
          sortStr  <- listToMaybe sortStrs :: Maybe BS.ByteString
          readSort sortStr
        filtTags = case Map.lookup "filterTag" params of 
          -- not-logged-in-case with tag specified
          Just fts -> [t | t <- map (fromFullName . decodeUtf8) $ fts
                         , tagPathIsElem t tags]
          Nothing -> 
            [fromFullName . snd . T.breakOnEnd "filterTag." 
             . decodeUtf8 . fst $ kv  
            | kv <- Map.toList params
            , T.isPrefixOf "filterTag." (decodeUtf8 . fst $ kv)
            , Map.lookup (fst kv) params == Just ("on":[])]
    in FiltSort sortCrit filtTags
          

allPaperRollSplices :: [Document] -> Splices (SnapletISplice App)
allPaperRollSplices docs = do
  "paper_roll_papers" ## (renderPaperRollPapers (take 100 docs))
  
renderPaperRollPapers :: [Document] -> SnapletISplice App
renderPaperRollPapers = I.mapSplices $ I.runChildrenWith . splicesFromDocument 

splicesFromDocument :: Document -> Splices (SnapletISplice App) 
splicesFromDocument doc = do
  let (novScore, rigScore, coolScore) = documentDimScores doc 
  "idNum"               ## I.textSplice (T.pack . show $ docId doc)
  "paper_title"         ## I.textSplice (docTitle doc)
  "paper_authors"       ## I.textSplice (T.intercalate ", " $ docAuthors doc)  
  "paper_external_link" ## I.textSplice (docLink doc)
  "noveltyScore"        ## I.textSplice (T.pack $ show (novScore))
  "rigorScore"          ## I.textSplice (T.pack $ show (rigScore))
  "coolnessScore"       ## I.textSplice (T.pack $ show (coolScore))
  (allDFieldTags $ docFieldTags doc)
 
allDFieldTags :: [TagPath] -> Splices (SnapletISplice App)
allDFieldTags tags = "fieldTags" ## renderDFieldTags fLabels
    where
      fLabels = map last tags

renderDFieldTags :: [T.Text] -> SnapletISplice App
renderDFieldTags = I.mapSplices $ I.runChildrenWith . splicesFromDTag 

splicesFromDTag :: Monad n => T.Text -> Splices (I.Splice n)
splicesFromDTag t = do
  "fieldTag" ## I.textSplice t
