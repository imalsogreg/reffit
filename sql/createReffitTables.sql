\i sql/user.sql

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

-- I think autherEquivalenceClass and autherEquivalenceClassMember
-- are subsumed by 'authors' and 'documentAuthors'. documentAuthors
-- are closely related to their parent document, and their nameString
-- comes from the document's spelling of their name.
-- Later, documentAuthors are collected by equivalence (through manual labelling)
-- into authors, when this happens, the docAuthor's nullable authorId is set to
-- a non-null authorId
--CREATE SEQUENCE authorEquivalenceClassIDSeq;
--CREATE TABLE authorEquivalenceClasses (
--       authorClassID int PRIMARY KEY DEFAULT nextval('authorEquivalenceClassIDSeq')
--);
--ALTER SEQUENCE authorEquivalenceClassIDSeq OWNED BY authorEquivalenceClasses.authorClassID;
--
--CREATE TABLE authorEquivalenceClassMembers (
--       authorID               int references reffitUsers(userID),
--       authorEquivalenceClass int references authorEquivalenceClasses(authorClassID),
--       UNIQUE (authorID, authorEquivalenceClass)
--);

CREATE SEQUENCE commentIDSeq;
CREATE TABLE comments (
       commentID     int PRIMARY KEY DEFAULT nextval('commentIDSeq'),
       commentTime   timestamptz,
       commentRating smallint,
       parentDoc     int references documents(documentID),
       parentComment int references comments(commentID),
       commentText   varchar(20000)
);
ALTER SEQUENCE commentIDSeq OWNED BY comments.commentID;

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
       voteComment   int references comments(commentID),
       voteValue     int NOT NULL,
       UNIQUE (voterID, voteDocument, voteComment),
       voteTime      timestamptz
);

CREATE TABLE anonVotes (
       voterID       int references reffitUsers(userID) NOT NULL,
       voteDocument  int references documents(documentID),
       voteComment   int references comments(commentID),
       voteValue     int NOT NULL,
       UNIQUE (voterID, voteDocument, voteComment),
       voteTime      timestamptz
);

\i sql/hashtag.sql
\i sql/documentViews.sql
