\COPY reffitUsers   FROM 'fakedata/reffitUsers.csv' WITH (FORMAT CSV);
\COPY emailAddys    FROM 'fakedata/emailAddys.csv' WITH (FORMAT CSV);
\COPY userFollowers FROM 'fakedata/userFollowers.csv' WITH (FORMAT CSV);
\COPY documents     FROM 'fakedata/documents.csv' WITH (FORMAT CSV);
\COPY comments      FROM 'fakedata/comments.csv' WITH (FORMAT CSV);
\COPY hashTags      FROM 'fakedata/hashTags.csv' WITH (FORMAT CSV);
\COPY hashTagMentions FROM 'fakedata/hashTagMentions.csv' WITH (FORMAT CSV);
