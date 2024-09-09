CREATE TABLE tbl_ryocom (
    nyugaikbn character(1) NOT NULL,
    ptid numeric(10,0) NOT NULL,
    sryym character(6) NOT NULL,
    zainum numeric(8,0) NOT NULL,
    zaiskbkbn character(1),
    srycd character(9),
    month numeric(2,0),
    day_1 numeric(2,0),
    day_2 numeric(2,0),
    day_3 numeric(2,0),
    day_4 numeric(2,0),
    day_5 numeric(2,0),
    day_6 numeric(2,0),
    day_7 numeric(2,0),
    day_8 numeric(2,0),
    day_9 numeric(2,0),
    day_10 numeric(2,0),
    day_11 numeric(2,0),
    day_12 numeric(2,0),
    day_13 numeric(2,0),
    day_14 numeric(2,0),
    day_15 numeric(2,0),
    day_16 numeric(2,0),
    day_17 numeric(2,0),
    day_18 numeric(2,0),
    day_19 numeric(2,0),
    day_20 numeric(2,0),
    day_21 numeric(2,0),
    day_22 numeric(2,0),
    day_23 numeric(2,0),
    day_24 numeric(2,0),
    day_25 numeric(2,0),
    day_26 numeric(2,0),
    day_27 numeric(2,0),
    day_28 numeric(2,0),
    day_29 numeric(2,0),
    day_30 numeric(2,0),
    day_31 numeric(2,0),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_ryocom
    ADD CONSTRAINT tbl_ryocom_primary_key PRIMARY KEY (hospnum, nyugaikbn, ptid, sryym, zainum);

