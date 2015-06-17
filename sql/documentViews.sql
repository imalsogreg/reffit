-- View of the json hashtag data for each document
-- Multiple within-doc occurrances of the same tag
-- will show up multiple times in the document's
-- results
CREATE VIEW documentHashTags AS
WITH nonZeros as (SELECT documents.documentID, array_to_json(array_agg(hashTags.hashTag)) as tags
                  FROM documents
                  INNER JOIN hashTags
                  ON documents.documentID = hashTags.docID
                  GROUP BY documents.documentID)
SELECT documents.documentID, COALESCE (tags, '[]'::json) as tags
FROM documents
LEFT JOIN nonZeros
ON documents.documentID = nonZeros.documentID
;

CREATE VIEW docAuths AS
WITH nonZeroDocs AS (
       SELECT row.documentID, array_to_json(array_agg(row)) as auths
       FROM documentAuthors row
       GROUP BY documentID)
SELECT documents.documentID, COALESCE (nonZeroDocs.auths, '[]'::json) as auths
FROM documents
LEFT JOIN nonZeroDocs
ON documents.documentID = nonZeroDocs.documentID
;

CREATE VIEW docUrls AS
WITH nonZeroDocs AS (
     SELECT row.documentID, array_to_json(array_agg(row)) as urls
     FROM documentUrls row
     GROUP BY documentID)
SELECT documents.documentID, COALESCE (nonZeroDocs.urls, '[]'::json) as urls
FROM documents
LEFT JOIN nonZeroDocs
ON documents.documentID = nonZeroDocs.documentID
;

CREATE VIEW documentNComments AS
WITH nonZeroCnts AS (
       SELECT parentDoc as documentID, count(commentID) as n
       FROM comments
       GROUP BY parentDoc
     ),
     zeroCnts AS (
       select documentID, 0 as n from documents
     )
(SELECT documentID, 0 as n from zeroCnts
 EXCEPT
 SELECT documentID, 0 as n from nonZeroCnts)
UNION select documentID,n from nonZeroCnts
;

CREATE VIEW documentNVotes AS
WITH legalValues AS (SELECT * from (values (1),(-1)) legalValues(voteValue)),
     nonZeroNs   AS (SELECT documentID, voteValue, count(*)
                     FROM documents
                     INNER JOIN votes
                     ON documents.documentID = votes.votedocument
                     GROUP BY documentID, voteValue),
     nNull       AS (SELECT documents.documentID, legalValues.voteValue
                     FROM documents
                     CROSS JOIN legalValues),
     nToplevel   AS ((SELECT documentID, votevalue, 0 as count from nNull
                      EXCEPT SELECT documentID, voteValue, 0 from nonZeroNs)
                     UNION
                     (SELECT documentid, votevalue, count from nonZeroNs))
SELECT * from nTopLevel;


CREATE VIEW documentSummary as
WITH docs      AS (select * from documents),
     tags      AS (select *
                   from documentHashTags),
     upVs      AS (SELECT * FROM documentNVotes WHERE votevalue = 1),
     dnVs      AS (SELECT * FROM documentNVotes WHERE votevalue = -1)
SELECT docs.documentID,  docs.title,
       reffitUsers.username, docAuths.auths,
       docUrls.urls, docs.docClass,
       docs.uploadTime,  documentNComments.n as nComments,
       upVs.count as uUpvotes, dnVs.count as nDownvotes, -- TODO Fix this
       tags.tags
FROM docs
LEFT JOIN documentNComments
ON docs.documentID = documentNComments.documentID
LEFT JOIN docAuths
ON docs.documentID = docAuths.documentID
LEFT JOIN reffitUsers
ON docs.docuploader = reffitUsers.userID
LEFT JOIN docUrls
ON docs.documentID = docUrls.documentID
LEFT JOIN upVs
ON docs.documentID = upVs.documentID
LEFT JOIN dnVs
ON docs.documentID = dnVs.documentID
LEFT JOIN tags
ON docs.documentID = tags.documentID
;
-- TODO make an index for documentSummary!

