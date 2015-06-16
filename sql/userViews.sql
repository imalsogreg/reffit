CREATE VIEW userReputation AS
WITH commentRep AS (SELECT comments.userID, sum(votes.voteValue)
                     FROM comments
                     INNER JOIN votes
                     ON votes.voteComment = comments.commentID
                     GROUP BY comments.commentID)
SELECT reffitUsers.userID, (COALESCE (sum(commentRep.sum), 0)) AS reputation
FROM reffitUsers
LEFT JOIN commentRep
ON reffitUsers.userID = commentRep.userID
GROUP BY reffitUsers.userID
;
