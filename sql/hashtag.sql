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
