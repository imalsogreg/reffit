CREATE TABLE hashTags (
       hashTag      varchar(200),
       docID         int references documents(documentID),
       commentID     int references comments(commentID)
);

CREATE SEQUENCE hashTagPageIDSeq;
CREATE TABLE IF NOT EXISTS  hashTagPages (
       hashTagPageID    int PRIMARY KEY DEFAULT nextval('hashTagPageIDSeq'),
       hashTagPageOwner int references reffitusers(userID),
       hashTag          varchar(200),
       pageText         varchar(20000),
       pageDate         timestamptz
);
ALTER SEQUENCE hashTagPageIDSeq OWNED BY hashTagPages.hashTagPageID;
