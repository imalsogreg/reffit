{-# LANGUAGE OverloadedStrings #-}

module HandleIndex(
  handleIndex
  )
where

import Reffit.Types
import PaperRoll
import Reffit.AcidTypes
import Reffit.FieldTag

import Control.Applicative
import Snap.Snaplet(Handler)
import Snap.Snaplet.AcidState (query)
import Snap.Snaplet.Auth
import Snap.Snaplet.Heist
import Application
import Heist
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import qualified Data.Map as Map 
import qualified Data.Set as Set
import Control.Monad

handleIndex :: Handler App (AuthManager App) ()
--handleIndex = handlePaperRoll
handleIndex = do
  docs  <- query QueryAllDocs
  us    <- query QueryAllUsers
  aUser <- currentUser

  let user' = join $ Map.lookup <$> (userLogin <$> aUser) <*> pure us :: Maybe User 
  renderWithSplices "_index" (allIndexSplices docs user' us)  

allIndexSplices :: Map.Map DocumentId Document -> Maybe User -> Map.Map UserName User -> Splices (SnapletISplice App)
allIndexSplices docs user' us = do
  allPaperRollSplices (Map.elems docs)
  allStatsSplices docs us
  case user' of 
    Nothing -> return ()
    Just user -> allFilterTagSplices user 


allStatsSplices :: Map.Map DocumentId Document -> Map.Map UserName User -> Splices (SnapletISplice App)
allStatsSplices docs us = do
  "nUsers"    ## I.textSplice $ T.pack . show . Map.size $ us
  "nDocs"     ## I.textSplice $ T.pack . show . Map.size $ docs
  "nComments" ## I.textSplice $ T.pack . show $
    sum (map (\d -> (Map.size . docSummaries $ d) + (Map.size . docCritiques $ d)) (Map.elems docs))
  "nVotes"    ## I.textSplice $ T.pack . show $
    sum (map (\d -> (sum $ map (length . summaryVotes) (Map.elems $ docSummaries d)) +
                    (sum $ map (length . critiqueReactions) (Map.elems $ docCritiques d))) (Map.elems docs)) 
    
allFilterTagSplices :: User -> Splices (SnapletISplice App)
allFilterTagSplices user = "fieldTags" ## renderFieldTags user (Set.toList $ userTags user)

renderFieldTags :: User -> [TagPath] -> SnapletISplice App
renderFieldTags user = I.mapSplices $ I.runChildrenWith . splicesFromFieldTag user

splicesFromFieldTag :: Monad n => User -> TagPath -> Splices (I.Splice n)
splicesFromFieldTag user tp = do
  "userName"         ## I.textSplice . userName $ user
  "fieldTagText"     ## I.textSplice . last $ tp
  "fieldTagFullText" ## I.textSplice . toFullName $ tp