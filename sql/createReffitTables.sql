\i sql/user.sql

CREATE SEQUENCE commentReferralIDSeq;
CREATE TABLE commentReferrals (
       referralID       int PRIMARY KEY DEFAULT nextval('commentReferralIDSeq'),
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

-- Authors with known names, may be associated with multiple docAuthors
CREATE SEQUENCE authorIDSeq;
CREATE TABLE authors (
  authorID int PRIMARY KEY DEFAULT nextval('authorIDSeq'),
  reffitID int references reffitUsers(userID),
  reffitVerified bool,
  authorGivenName varchar(200),
  authorSurname   varchar(200)
);
ALTER SEQUENCE authorIDSeq OWNED BY authors.authorID;

-- docAuthor is a particular spelling of a name used in a single paper
-- May get manually associated with a single 'real' author
CREATE TABLE documentAuthors (
       authorID        int,
       documentID      int    references documents(documentID),
       nameString    varchar(200),
       UNIQUE (authorID, documentID)
);


CREATE SEQUENCE commentIDSeq;
CREATE TABLE comments (
       commentID     int PRIMARY KEY DEFAULT nextval('commentIDSeq'),
       userID        int references reffitUsers(userID),
       commentTime   timestamptz,
       commentRating smallint,
       parentDoc     int references documents(documentID),
       parentComment int references comments(commentID),
       commentText   varchar(20000)
);
ALTER SEQUENCE commentIDSeq OWNED BY comments.commentID;

CREATE TABLE privateCommentAuthors (
       commentID int references comments(commentID),
       userID    int references reffitUsers(userID)
);

CREATE SEQUENCE voteIDSeq;
CREATE TABLE votes (
       voteID        int PRIMARY KEY DEFAULT nextval('voteIDSeq'),
       userID        int references reffitUsers(userID),
       voteDocument  int references documents(documentID),
       voteComment   int references comments(commentID),
       voteValue     int NOT NULL,
       UNIQUE (userID, voteDocument, voteComment),
       voteTime      timestamptz
);
ALTER SEQUENCE voteIDSeq OWNED BY votes.voteID;

CREATE TABLE privateVotes (
       userID        int references reffitUsers(userID) NOT NULL,
       voteID        int references votes(voteID)
);

\i sql/hashtag.sql
\i sql/documentViews.sql
\i sql/userViews.sql
