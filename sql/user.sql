CREATE SEQUENCE reffitUserIDSeq;
CREATE TABLE reffitUsers (
       userID       smallint PRIMARY KEY DEFAULT nextval('reffitUserIDSeq'),
       userName     varchar(200) UNIQUE,
       userJoinTime timestamptz
);
ALTER SEQUENCE reffitUserIDSeq OWNED BY reffitUsers.userID;

CREATE TABLE userFollowers (
       follower   int references reffitusers(userid),
       followed   int references reffitusers(userid),
       UNIQUE (follower,followed),
       followTime timestamptz
);

CREATE TABLE emailAddys (
       emailAddy  varchar(200),
       userID     int           references reffitUsers(userID),
       verified   boolean,
       verifiedAt timestamptz,
       verifyKey  varchar(200),
       isPrimary  boolean,
       UNIQUE (userID, emailAddy)
);

CREATE SEQUENCE passwordResetRequestIDSeq;
CREATE TABLE passwordResetRequests (
       resetRequestID int PRIMARY KEY DEFAULT nextval ('passwordResetRequestIDSeq'),
       userID         int references reffitUsers(userID),
       resetKey       varchar(200),
       resetExpiresAt timestamptz
);
ALTER SEQUENCE passwordResetRequestIDSeq OWNED BY passwordResetRequests.resetRequestID;
