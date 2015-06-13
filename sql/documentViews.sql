CREATE VIEW documentSummary as
WITH docs      AS (select * from documents),
     coms      AS (select count(*)
                   from comments, docs
                   where comments.parentDoc=docs.documentID),
     tags      AS (select * from hashTags)
SELECT docs.documentID,  docs.title,
       docs.docUploader, docs.docClass,
       docs.uploadTime,  coms,
       tags
FROM docs, coms, tags;
-- TODO make an index for documentSummary!

-- View of the json hashtag data for each document
-- Multiple within-doc occurrances of the same tag
-- will show up multiple times in the document's
-- results
CREATE VIEW documentHashTags AS
SELECT mentions.docID, array_to_json(array_agg(mentions))
FROM (SELECT docID, tag.hashTagID, tag.hashTag, tag.hashTagDescription
      FROM   hashTagMentions
      LEFT JOIN hashTags as tag
      ON tag.hashTagID = hashTagMentions.hashTag) as mentions
GROUP BY mentions.docID
;

CREATE VIEW documentNComments AS
SELECT parentDoc, count(commentID)
FROM comments
GROUP BY parentDoc
;
