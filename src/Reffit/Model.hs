{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Reffit.Model where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql (runSqlite, runMigration)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
                            share, sqlSettings)

data FlagType = FlagOffensive
              | FlagMisleadingEdit
              | FlagMisleadingUser
              | FlagSpam
              deriving (Read, Show)

data

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  UserName      Text
  userPic
UserRealName
  userRealName     Text
  userRealNameUser UserID
UserEmail
  userEmailUser         UserID
  userEmail             Text
  userEmailVerififyCode Int
  userEmailVerified     Bool
UserFollower
  userFollowerFollower UserID
  userFollowerFollowed UserID
UserActivity
  userActivityUser   UserID
  userActivityTime   UTCTime
  userActivityShow   Bool
  userActivityJSON   Text  --TODO: Serialize user-activity ADT through JSON
  userActivityHTML   Text

Document
  docAuthors         Text -- comma-separated list of usernames
  docTitle           Text
  docSumbitter       UserName
  docParticipants    Text -- comma-separated list of usernames
  docTags            Text -- comma-separated list of tags
  docLink            Text
  docOpenHtmlLink    Text
  docOpenPdfLink     Text
  docPostTime        UTCTime
  docClass           --TODO: DocClassEnum

Pinboard
  pinboardUser UserID
  pinboardText Text
  pinboardHTML Text

UserPinnedDoc
  pinnedDoc DocumentID
  docPinner UserID
  pinNotes  Text

DocAuthor
  docAuthorName      Text
DocAuthorUserClaim
  docAuthorUserClaim    UserID
  docAuthorUserVerified Bool
DocFlag
  docFlagDoc  DocumentID
  docFlagType FlagType -- TODO: How to derive sum type here?
  docFlagUser UserID
  docFlagTime UTCTime

DocComment
  docCommentDoc    DocumentID
  docCommentRaw    Text
  docCommentHtml   Text
  docCommentUser   UserID

DocCommentVote
  docCommentVoteType    VoteTypeEnum --TODO
  docCommentVoteUser    UserID
  docCommentVoteDoc     DocumentID
  docCommentVoteComment DocCommentID
  
|]
