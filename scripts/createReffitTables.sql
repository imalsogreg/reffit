CREATE TABLE reffitUsers (
       userID       int PRIMARY KEY,
       userName     varchar(200) UNIQUE,
       userJoinTime timestamp
);

CREATE TABLE userFollowers (
       follower   int references reffitusers(userid),
       followed   int references reffitusers(userid),
       followTime timestamp
);

CREATE TABLE emailAddys (
       emailID    int           PRIMARY KEY,
       emailAddy  varchar(200),
       userID     int           references reffitUsers(userID),
       verified   boolean,
       verifiedAt timestamp,
       verifyKey  varchar(200),
       isPrimary  boolean
);

CREATE TABLE passwordResetRequests (
       resetRequestID int          PRIMARY KEY,
       userID         int          references reffitUsers(userID),
       resetKey       varchar(200),
       resetExpiresAt timestamp
);

CREATE TABLE commentReferrals (
       referralID    int          PRIMARY KEY,
       referrer      int          references reffitUsers(userID),
       referredEmail varchar(200),
       referredUserName varchar(200), 
       accepted      smallint
);

CREATE TABLE documents (
       documentID   int           PRIMARY KEY,
       title        varchar(400),
       docUploader  int           references reffitusers(userid),
       docClass     varchar(100),
       uploadTime   timestamp,
       docSourceURL varchar(200)
);

CREATE TABLE userPinboard (
       userID  int references reffitUsers(userID),
       docID   int references documents(documentID),
       pinTime timestamp
);

CREATE TABLE documentAuthors (
       authorID int          PRIMARY KEY,
       document int          references documents(documentID),
       author   varchar(200)
);

CREATE TABLE authors (
       authorID int       PRIMARY KEY,
       authorReffitID int references reffitUsers(userID)
);     

CREATE TABLE comments (
       commentID   int           PRIMARY KEY,
       commentTime timestamp,
       parentDoc      int           references documents(documentID),
       parentComment  int           references comments(commentID),
       commentText varchar(50000)
);

CREATE TABLE commentParts (
       commentPartID  int            UNIQUE PRIMARY KEY,
       wholeCommentID int            references comments(commentID),
       ratingOfPaper  smallint,
       partIndex      smallint,
       text           varchar(10000),
       html           varchar(50000)
);

CREATE TABLE publicCommentAuthors (
       commentID       int references comments(commentID),
       authorID        int references reffitUsers(userID)
);

CREATE TABLE anonCommentAuthors (
       commentID     int references comments(commentID),
       authorID      int references reffitUsers(userID)
);

CREATE TABLE publicCommentVotes (
       voterID       int references reffitUsers(userID),
       voteSubject   int references commentParts(commentPartID),
       voteValue     int NOT NULL,
       voteTime      timestamp
);

CREATE TABLE anonCommentVotes (
       voterID       int references reffitUsers(userID),
       voteSubject   int references commentParts(commentPartID),
       voteValue     int NOT NULL,
       voteTime      timestamp
);

CREATE TABLE hashTags (
       hashTagID int PRIMARY KEY,
       hashTagFullName  varchar(200),
       hashTagShortName varchar(50)
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
       pageDate        timestamp
);

