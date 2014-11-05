CREATE TABLE reffitUsers (
       userID       int PRIMARY KEY,
       userName     varchar(200) UNIQUE,
       userJoinTime timestamptz
);

CREATE TABLE userFollowers (
       follower   int references reffitusers(userid),
       followed   int references reffitusers(userid),
       UNIQUE (follower,followed),
       followTime timestamptz
);

CREATE TABLE emailAddys (
       emailID    int           PRIMARY KEY,
       emailAddy  varchar(200),
       userID     int           references reffitUsers(userID),
       verified   boolean,
       verifiedAt timestamptz,
       verifyKey  varchar(200),
       isPrimary  boolean
);

CREATE TABLE passwordResetRequests (
       resetRequestID int          PRIMARY KEY,
       userID         int          references reffitUsers(userID),
       resetKey       varchar(200),
       resetExpiresAt timestamptz
);

CREATE TABLE commentReferrals (
       referralID       int          PRIMARY KEY,
       referrer         int          references reffitUsers(userID),
       referredEmail    varchar(200),
       referredUserName varchar(200), 
       accepted         smallint
);

CREATE TABLE documents (
       documentID   int           PRIMARY KEY,
       title        varchar(400),
       docUploader  int           references reffitusers(userid),
       docClass     varchar(100),
       uploadTime   timestamptz
);

CREATE TABLE documentURLs (
       documentID int references  documents(documentID),
       docSourceURL varchar(400)
);

CREATE TABLE userPinboard (
       userID  int references reffitUsers(userID),
       docID   int references documents(documentID),
       pinTime timestamptz
);

CREATE TABLE authors (
  authorID int PRIMARY KEY,
  reffitID int references reffitUsers(userID),
  authorGivenName varchar(200),
  authorSurname   varchar(200)
);

CREATE TABLE documentAuthors (
       authorID        int    PRIMARY KEY,
       documentID      int    references documents(documentID)
);


CREATE TABLE authorEquivalenceClasses (
       authorClassID int PRIMARY KEY
);

CREATE TABLE authorEquivalenceClassMembers (
       authorID               int references reffitUsers(userID),
       authorEquivalenceclass int references authorEquivalenceClasses(authorClassID)
);

CREATE TABLE comments (
       commentID   int           PRIMARY KEY,
       commentTime timestamptz,
       parentDoc      int           references documents(documentID),
       commentText varchar(50000)
);

CREATE TABLE commentParts (
       commentPartID     int           UNIQUE PRIMARY KEY,
       wholeCommentID    int           references comments(commentID),
       commentRating     smallint,
       partIndex         smallint,
       parentComment     int           references comments(commentID),
       parentCommentPart int           references commentParts(commentPartID),
       text              varchar(10000),
       html              varchar(50000)
);

CREATE TABLE publicCommentAuthors (
       commentID       int references comments(commentID),
       authorID        int references reffitUsers(userID)
       UNIQUE (commentID, authorID)
);

CREATE TABLE anonCommentAuthors (
       commentID     int references comments(commentID),
       authorID      int references reffitUsers(userID)
       UNIQUE (commentID, authorID)
);

CREATE TABLE publicCommentVotes (
       voterID       int references reffitUsers(userID) NOT NULL,
       voteSubject   int references commentParts(commentPartID),
       voteValue     int NOT NULL,
       voteTime      timestamptz
);

CREATE TABLE anonCommentVotes (
       voterID       int references reffitUsers(userID) NOT NULL,
       voteSubject   int references commentParts(commentPartID),
       voteValue     int NOT NULL,
       voteTime      timestamptz
);

CREATE TABLE hashTags (
       hashTagLongName  varchar (200) PRIMARY KEY,
       hashTagShortName varchar(50),
       hashTagParent    varchar (200) references hashTagLongName
);

CREATE TABLE hashTagMentions (
       hashMentionID int PRIMARY KEY,
       hashTag       int references hashTags(hashTagID),
       docID         int references documents(documentID),
       commentID     int references comments(commentID),
       commentPartID int references commentParts(commentPartID)
);

CREATE TABLE hashTagPages (
       hashTagPageID   int PRIMARY KEY,
       hashTag         int references hashTags(hashTagID),
       pageText        varchar(20000),
       pageHtml        varchar(40000),
       pageDate        timestamptz
);

