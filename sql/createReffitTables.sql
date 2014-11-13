CREATE SEQUENCE reffitUserIDSeq;
CREATE TABLE reffitUsers (
       userID       smallint PRIMARY KEY DEFAULT nextval('reffitUserIDSeq'),
       userName     varchar(200) UNIQUE,
       userJoinTime timestamptz
);
ALTER SEQUENCE reffitUserIDSeq OWNED BY reffitUsers.userID;

CREATE TABLE userFollowers (
       follower   int references reffitusers(userid),
       followed   int references reffitusers(userid),
       UNIQUE (follower,followed),
       followTime timestamptz
);

CREATE TABLE emailAddys (
       emailAddy  varchar(200),
       userID     int           references reffitUsers(userID),
       verified   boolean,
       verifiedAt timestamptz,
       verifyKey  varchar(200),
       isPrimary  boolean,
       UNIQUE (userID, emailAddy)
);

CREATE SEQUENCE passwordResetRequestIDSeq;
CREATE TABLE passwordResetRequests (
       resetRequestID int PRIMARY KEY DEFAULT nextval ('passwordResetRequestIDSeq'),
       userID         int references reffitUsers(userID),
       resetKey       varchar(200),
       resetExpiresAt timestamptz
);
ALTER SEQUENCE passwordResetRequestIDSeq OWNED BY passwordResetRequests.resetRequestID;

CREATE SEQUENCE commentReferralIDSeq;
CREATE TABLE commentReferrals (
       referralID       int PRIMARY KEY DEFAULt nextval('commentReferralIDSeq'),
       referrer         int references reffitUsers(userID),
       referredEmail    varchar(200),
       referredUserName varchar(200), 
       accepted         smallint
);
ALTER SEQUENCE commentReferralIDSeq OWNED BY commentReferrals.referralID;

CREATE SEQUENCE documentIDSeq;
CREATE TABLE documents (
       documentID   int           PRIMARY KEY DEFAULT nextval('documentIDSeq'),
       title        varchar(400),
       docUploader  int           references reffitusers(userid),
       docClass     varchar(100),
       uploadTime   timestamptz
);
ALTER SEQUENCE documentIDSeq OWNED BY documents.documentID;

CREATE TABLE documentURLs (
       documentID int references  documents(documentID),
       docSourceURL varchar(400),
       UNIQUE (documentID, docSourceURL)
);

CREATE TABLE userPinboard (
       userID  int references reffitUsers(userID),
       docID   int references documents(documentID),
       pinTime timestamptz,
       UNIQUE (userID, docID)
);

CREATE SEQUENCE authorIDSeq;
CREATE TABLE authors (
  authorID int PRIMARY KEY DEFAULT nextval('authorIDSeq'),
  reffitID int references reffitUsers(userID),
  authorGivenName varchar(200),
  authorSurname   varchar(200)
);
ALTER SEQUENCE authorIDSeq OWNED BY authors.authorID;

CREATE TABLE documentAuthors (
       authorID        int    PRIMARY KEY,
       documentID      int    references documents(documentID),
       UNIQUE (authorID, documentID)
);

CREATE SEQUENCE authorEquivalenceClassIDSeq;
CREATE TABLE authorEquivalenceClasses (
       authorClassID int PRIMARY KEY DEFAULT nextval('authorEquivalenceClassIDSeq')
);
ALTER SEQUENCE authorEquivalenceClassIDSeq OWNED BY authorEquivalenceClasses.authorClassID;

CREATE TABLE authorEquivalenceClassMembers (
       authorID               int references reffitUsers(userID),
       authorEquivalenceClass int references authorEquivalenceClasses(authorClassID),
       UNIQUE (authorID, authorEquivalenceClass)
);

CREATE SEQUENCE commentIDSeq;
CREATE TABLE comments (
       commentID   smallint PRIMARY KEY DEFAULT nextval('commentIDSeq'),
       commentTime timestamptz,
       parentDoc      int           references documents(documentID),
       commentText varchar(50000)
);
ALTER SEQUENCE commentIDSeq OWNED BY comments.commentID;


CREATE SEQUENCE commentPartIDSeq;
CREATE TABLE commentParts (
       commentPartID     smallint  PRIMARY KEY DEFAULT nextval('commentPartIDSeq'),
       wholeCommentID    smallint references comments(commentID),
       commentRating     smallint,
       partIndex         smallint,
       parentComment     smallint,
       foreign key (parentComment)          references comments(commentID),
       parentCommentPart smallint      references commentParts(commentPartID),
       text              varchar(10000),
       html              varchar(50000)
);
ALTER SEQUENCE commentPartIDSeq OWNED BY commentParts.commentPartID;

CREATE TABLE publicCommentAuthors (
       commentID       int references comments(commentID),
       authorID        int references reffitUsers(userID),
       UNIQUE (commentID, authorID)
);

CREATE TABLE anonCommentAuthors (
       commentID     int references comments(commentID),
       authorID      int references reffitUsers(userID),
       UNIQUE (commentID, authorID)
);

CREATE TABLE publicVotes (
       voterID       int references reffitUsers(userID) NOT NULL,
       voteDocument  int references documents(documentID),
       voteComment   int references commentParts(commentPartID),
       voteValue     int NOT NULL,
       UNIQUE (voterID, voteDocument, voteComment),
       voteTime      timestamptz
);

CREATE TABLE anonVotes (
       voterID       int references reffitUsers(userID) NOT NULL,
       voteDocument  int references documents(documentID),
       voteComment   int references commentParts(commentPartID),
       voteValue     int NOT NULL,
       UNIQUE (voterID, voteDocument, voteComment),
       voteTime      timestamptz
);

CREATE TABLE hashTags (
       hashTagLongName  varchar (200) PRIMARY KEY,
       hashTagShortName varchar(50),
       hashTagParent    varchar(200) references hashTags(hashTagLongName)
);

CREATE SEQUENCE hashTagMentionsSeq;
CREATE TABLE hashTagMentions (
       hashMentionID int PRIMARY KEY DEFAULT nextval('hashTagMentionsSeq'),
       hashTag       varchar(200) references hashTags(hashTagLongName),
       docID         int references documents(documentID),
       commentID     int references comments(commentID),
       commentPartID int references commentParts(commentPartID)
);
ALTER SEQUENCE hashTagMentionsSeq OWNED BY hashTagMentions.hashMentionID;

CREATE SEQUENCE hashTagPageIDSeq;
CREATE TABLE hashTagPages (
       hashTagPageID   int PRIMARY KEY DEFAULT nextval('hashTagPageIDSeq'),
       hashTag         varchar(200) references hashTags(hashTagLongName),
       pageText        varchar(20000),
       pageHtml        varchar(40000),
       pageDate        timestamptz
);
ALTER SEQUENCE hashTagPageIDSeq OWNED BY hashTagPages.hashTagPageID;
