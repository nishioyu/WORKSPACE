CREATE TABLE tbl_syurrk (
    hospnum numeric(2,0) NOT NULL,
    ptid numeric(10,0) NOT NULL,
    nyugaikbn character(1) NOT NULL,
    denpnum numeric(7,0) NOT NULL,
    syurrknum numeric(2,0) NOT NULL,
    syuedanum numeric(2,0) NOT NULL,
    joutaikbn character(1),
    sryka character(2),
    hkncombinum character(4),
    ssu_hknten numeric(7,0),
    ssu_money numeric(7,0),
    ssu_tgmoney numeric(7,0),
    ssu_tgmoney_tax numeric(7,0),
    sdo_hknten numeric(7,0),
    sdo_money numeric(7,0),
    sdo_tgmoney numeric(7,0),
    sdo_tgmoney_tax numeric(7,0),
    ztk_hknten numeric(7,0),
    ztk_money numeric(7,0),
    ztk_tgmoney numeric(7,0),
    ztk_tgmoney_tax numeric(7,0),
    tyk_hknten numeric(7,0),
    tyk_money numeric(7,0),
    tyk_tgmoney numeric(7,0),
    tyk_tgmoney_tax numeric(7,0),
    csy_hknten numeric(7,0),
    csy_money numeric(7,0),
    csy_tgmoney numeric(7,0),
    csy_tgmoney_tax numeric(7,0),
    syc_hknten numeric(7,0),
    syc_money numeric(7,0),
    syc_tgmoney numeric(7,0),
    syc_tgmoney_tax numeric(7,0),
    sjt_hknten numeric(7,0),
    sjt_money numeric(7,0),
    sjt_tgmoney numeric(7,0),
    sjt_tgmoney_tax numeric(7,0),
    msi_hknten numeric(7,0),
    msi_money numeric(7,0),
    msi_tgmoney numeric(7,0),
    msi_tgmoney_tax numeric(7,0),
    kns_hknten numeric(7,0),
    kns_money numeric(7,0),
    kns_tgmoney numeric(7,0),
    kns_tgmoney_tax numeric(7,0),
    gzu_hknten numeric(7,0),
    gzu_money numeric(7,0),
    gzu_tgmoney numeric(7,0),
    gzu_tgmoney_tax numeric(7,0),
    etc_hknten numeric(7,0),
    etc_money numeric(7,0),
    etc_tgmoney numeric(7,0),
    etc_tgmoney_tax numeric(7,0),
    ssn_hknten numeric(7,0),
    ssn_money numeric(7,0),
    ssn_tgmoney numeric(7,0),
    ssn_tgmoney_tax numeric(7,0),
    hou_hknten numeric(7,0),
    hou_money numeric(7,0),
    hou_tgmoney numeric(7,0),
    hou_tgmoney_tax numeric(7,0),
    nyn_hknten numeric(7,0),
    nyn_money numeric(7,0),
    nyn_tgmoney numeric(7,0),
    nyn_tgmoney_tax numeric(7,0),
    ryo_hknten numeric(7,0),
    ryo_money numeric(7,0),
    ryo_tgmoney numeric(7,0),
    ryo_tgmoney_tax numeric(7,0),
    total_hknten numeric(7,0),
    total_money numeric(7,0),
    total_tgmoney numeric(7,0),
    total_tgmoney_tax numeric(7,0),
    shohou_sai numeric(7,0),
    tgmoney_tax_sai numeric(7,0),
    jihi_1 numeric(7,0),
    jihi_1_tax numeric(7,0),
    jihi_2 numeric(7,0),
    jihi_2_tax numeric(7,0),
    jihi_3 numeric(7,0),
    jihi_3_tax numeric(7,0),
    jihi_4 numeric(7,0),
    jihi_4_tax numeric(7,0),
    jihi_5 numeric(7,0),
    jihi_5_tax numeric(7,0),
    jihi_6 numeric(7,0),
    jihi_6_tax numeric(7,0),
    jihi_7 numeric(7,0),
    jihi_7_tax numeric(7,0),
    jihi_8 numeric(7,0),
    jihi_8_tax numeric(7,0),
    jihi_9 numeric(7,0),
    jihi_9_tax numeric(7,0),
    jihi_10 numeric(7,0),
    jihi_10_tax numeric(7,0),
    jihi_total numeric(7,0),
    jihi_total_tax numeric(7,0),
    jihi_tax numeric(7,0),
    rjn_ftn numeric(7,0),
    koh_ftn numeric(7,0),
    ykz_ftn numeric(7,0),
    chosei numeric(7,0),
    hkntekmoney numeric(7,0),
    discount_kbn character(2),
    discount_money numeric(7,0),
    rsishoshin_money numeric(7,0),
    rsisaishin_money numeric(7,0),
    rsisdo_money numeric(7,0),
    rsietc_money numeric(7,0),
    rsi_tax_sai numeric(7,0),
    rsi_total numeric(7,0),
    rsijibai_skymoney numeric(7,0),
    skymoney_tax_sai numeric(7,0),
    skymoney numeric(7,0),
    rmsagaku numeric(7,0),
    rmsagaku_tax_sai numeric(7,0),
    ryoyohi_skj numeric(7,0),
    skymoney_skj numeric(7,0),
    skymoney_skj_tax numeric(7,0),
    skymoney_skj_kei numeric(7,0),
    ryoyohi_skj_jihi numeric(7,0),
    skymoney_skj_jihi numeric(7,0),
    skymoney_skj_jihi_tax numeric(7,0),
    skymoney_skj_jihi_kei numeric(7,0),
    ryoyohi_life numeric(7,0),
    skymoney_life numeric(7,0),
    skymoney_life_tax numeric(7,0),
    skymoney_life_kei numeric(7,0),
    ryoyohi_life_jihi numeric(7,0),
    skymoney_life_jihi numeric(7,0),
    skymoney_life_jihi_tax numeric(7,0),
    skymoney_life_jihi_kei numeric(7,0),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    byr_hknten numeric(7,0) DEFAULT 0,
    byr_money numeric(7,0) DEFAULT 0,
    byr_tgmoney numeric(7,0) DEFAULT 0,
    byr_tgmoney_tax numeric(7,0) DEFAULT 0,
    chosei1 integer DEFAULT 0,
    chosei2 integer DEFAULT 0
);

ALTER TABLE ONLY tbl_syurrk
    ADD CONSTRAINT tbl_syurrk_primary_key PRIMARY KEY (hospnum, ptid, nyugaikbn, denpnum, syurrknum, syuedanum);

