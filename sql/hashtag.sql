CREATE SEQUENCE hashTagSeq;
CREATE TABLE hashTags (
       hashTagID          int PRIMARY KEY DEFAULT nextval('hashTagSeq'),
       hashTag            varchar(50),
       hashTagDescription varchar(200)
);
ALTER SEQUENCE hashTagSeq OWNED BY hashTags.hashTagID;

CREATE SEQUENCE hashTagMentionsSeq;
CREATE TABLE hashTagMentions (
       hashMentionID int PRIMARY KEY DEFAULT nextval('hashTagMentionsSeq'),
       hashTag       int references hashTags(hashTagID),
       docID         int references documents(documentID),
       commentID     int references comments(commentID)
);
ALTER SEQUENCE hashTagMentionsSeq OWNED BY hashTagMentions.hashMentionID;

CREATE SEQUENCE hashTagPageIDSeq;
CREATE TABLE IF NOT EXISTS  hashTagPages (
       hashTagPageID   int PRIMARY KEY DEFAULT nextval('hashTagPageIDSeq'),
       hashTag         int references hashTags(hashTagID),
       pageText        varchar(20000),
       pageHtml        varchar(40000),
       pageDate        timestamptz
);
ALTER SEQUENCE hashTagPageIDSeq OWNED BY hashTagPages.hashTagPageID;
