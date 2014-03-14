module Reffit.Model where

import Data.Text Text
import Database.Persist
import Database.Persist.Sqline (runSqlite, runMigration)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
                            share, sqlSettings)




share [mkPersist sqlSettings, mkMigrate "migrateTables"]
      [peristLowerCase|
ViewPrefs
User
  UserName      Text
  realNames     Text -- comma-separated list
  emails        Text -- comma-separated list
  claimedEmails Text -- comma-separated list
  affil         Text
  following     Text -- comma-separated list of usernames
  userPic
Author
  authorName         Text
  claimedBy          Text -- comma-separated list of usernames
  verifiedAuthorUser (Maybe userName)
