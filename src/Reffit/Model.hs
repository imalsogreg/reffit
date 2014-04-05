{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Reffit.Model where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
                            share, sqlSettings)

data FlagType = FlagOffensive
              | FlagMisleadingEdit
              | FlagMisleadingUser
              | FlagSpam
              deriving (Read, Show)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  UserName      Text
  userPic
  userRealName     Text
UserEmail
  userEmailUser         UserID
  userEmail             Text
  userEmailVerififyCode Int
  userEmailVerified     Bool
UserFollower
  userFollowerFollower UserID
  userFollowerFollowed UserID

Document
  docTitle           Text
  docSumbitter       UserName
  docTags            Text -- comma-separated list of tags
  docLink            Text
  docOpenHtmlLink    Text
  docOpenPdfLink     Text
  docPostTime        UTCTime
  docClass           DocClassEnum

DocAuthor
  docId             DocumentID
  docAuthorName     Text
  docAuthorUsername Text
  docAuthorVerified Bool

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
  docFlagType FlagType
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

