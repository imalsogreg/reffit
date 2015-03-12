\i user.sql

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
       commentID     smallint PRIMARY KEY DEFAULT nextval('commentIDSeq'),
       commentTime   timestamptz,
       parentDoc     int           references documents(documentID),
       parentComment int,
       foreign key   parentComment references comments(commentID),
       commentText   varchar(50000)
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

\i hashtag.sql
