{-# LANGUAGE QuasiQuotes, TypeFamilies
, GeneralizedNewtypeDeriving, TemplateHaskell
, OverloadedStrings, GADTs
, FlexibleContexts #-}

module Reffit.PersistentTypes where



share [mkPersiste sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Vote
  voteOn DocumentId
  
