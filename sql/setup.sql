CREATE TABLE User (
    id integer NOT NULL AUTO_INCREMENT,
    user_email varchar(100) NOT NULL,
    name varchar(100) NOT NULL,
    surname varchar(100) NOT NULL,
    password varchar(100) NOT NULL,
    is_admin boolean NOT NULL DEFAULT FALSE,
    PRIMARY KEY (id)
);

CREATE TABLE Equipment (
    equip_id integer NOT NULL AUTO_INCREMENT,
    name varchar(255) NOT NULL,
    status enum('Работает','Изношено','Не работает'),
    user_id integer,
    warranty DATE,
    FOREIGN KEY (user_id) REFERENCES User(id),
    PRIMARY KEY (equip_id)
);

CREATE TABLE Record (
    record_id integer NOT NULL AUTO_INCREMENT,
    type enum('Хранение','Передача','Ремонт','Списание'),
    place varchar(255),
    date_begin date NOT NULL,
    date_end date,
    old_owner integer,
    new_owner integer,
    reason varchar(255),
    equip_id integer,
    add_date date NOT NULL,
    FOREIGN KEY (equip_id) REFERENCES Equipment(equip_id),
    FOREIGN KEY (new_owner) REFERENCES User(id),
    FOREIGN KEY (old_owner) REFERENCES User(id),
    PRIMARY KEY (record_id)
);

CREATE TABLE License(
    license_id integer NOT NULL AUTO_INCREMENT,
    license_name varchar(255) NOT NULL,
    file_name varchar(255) NOT NULL,
    date_end DATE NOT NULL,
    PRIMARY KEY (license_id)
);