-- Users schema

-- !Ups

CREATE TABLE User (
    id integer NOT NULL AUTO_INCREMENT,
    user_email varchar(100) NOT NULL,
    name varchar(100) NOT NULL,
    surname varchar(100) NOT NULL,
    password varchar(100) NOT NULL,
    is_admin boolean NOT NULL DEFAULT FALSE,
    PRIMARY KEY (id)
);

-- !Downs

DROP TABLE User;