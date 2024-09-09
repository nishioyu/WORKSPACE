--                                    --
-- ��Ǽ�ơ��֥���ѹ�                 --
--                                    --
-- ���ܤ��ɲ�                         --
--       �����оݳ������б���ʡ����   --
--          KYUFUGAI-SHOSHIN-TEN      --
--          KYUFUGAI-SIDOU-TEN        --
--          KYUFUGAI-OSIN1-TEN        --
--          KYUFUGAI-OSIN2-TEN        --
--          KYUFUGAI-OSIN3-TEN        --
--          KYUFUGAI-GOKEI-TEN        --
--                                    --
-- Create Date : 2003/08/21           --
--                                    --

-- TBL_SYUNOUWK ����                  --

create table TBL_SYUNOUWK (
HOSPID				char(24)	not null,
NYUGAIKBN			char(01)	not null,
PTID				numeric(10)     not null,
DENPNUM   			numeric(07)     not null,
DENPLASTNUM  			numeric(02),
SRYKA                           char(02),
SRYYMD                          char(08),
DENPJTIKBN                      char(01),
CREATEKBN                       char(01),
DENPPRTYMD    			char(08),
SKYSTYMD                        char(08),
SKYEDYMD                        char(08),
HKNCOMBINUM                     char(04),
SYUHKNNUM                       char(03),
SYUHKNFTNMONEY                  numeric(07),
SYUCOMPFTN                      numeric(07),
SYUCOMPYKZFTN                   numeric(07),
SYUCOMPTOTAL                    numeric(07),
SYUYKZFTNKBN                    char(01),
KOH1HKNNUM                      char(03),
KOH1HKNFTNMONEY                 numeric(07),
KOH1COMPFTN                     numeric(07),
KOH1COMPYKZFTN                  numeric(07),
KOH1COMPTOTAL                   numeric(07),
KOH1YKZFTNKBN                   char(01),
KOH2HKNNUM                      char(03),
KOH2HKNFTNMONEY                 numeric(07),
KOH2COMPFTN                     numeric(07),
KOH2COMPYKZFTN                  numeric(07),
KOH2COMPTOTAL                   numeric(07),
KOH2YKZFTNKBN                   char(01),
KOH3HKNNUM                      char(03),
KOH3HKNFTNMONEY                 numeric(07),
KOH3COMPFTN                     numeric(07),
KOH3COMPYKZFTN                  numeric(07),
KOH3COMPTOTAL                   numeric(07),
KOH3YKZFTNKBN                   char(01),
KOH4HKNNUM                      char(03),
KOH4HKNFTNMONEY                 numeric(07),
KOH4COMPFTN                     numeric(07),
KOH4COMPYKZFTN 			numeric(07),
KOH4COMPTOTAL                   numeric(07),
KOH4YKZFTNKBN                   char(01),
PTFTNRATE                       numeric(03),
ROOMNUM                         char(08),
BYOTONUM                        char(02),
SKYKUKBN                        char(01),
TAX_TAISHOU                     numeric(07),
TAX_MONEY                       numeric(07),
SKYGK                           numeric(07),
SSU_HKNTEN                      numeric(07),
SSU_MONEY                       numeric(07),
SSU_TGMONEY                     numeric(07),
SSU_TGMONEY_TAX                 numeric(07),
SDO_HKNTEN                      numeric(07),
SDO_MONEY                       numeric(07),
SDO_TGMONEY                     numeric(07),
SDO_TGMONEY_TAX                 numeric(07),
ZTK_HKNTEN                      numeric(07),
ZTK_MONEY                       numeric(07),
ZTK_TGMONEY                     numeric(07),
ZTK_TGMONEY_TAX                 numeric(07),
TYK_HKNTEN                      numeric(07),
TYK_MONEY                       numeric(07),
TYK_TGMONEY                     numeric(07),
TYK_TGMONEY_TAX                 numeric(07),
CSY_HKNTEN                      numeric(07),
CSY_MONEY                       numeric(07),
CSY_TGMONEY                     numeric(07),
CSY_TGMONEY_TAX                 numeric(07),
SYC_HKNTEN                      numeric(07),
SYC_MONEY                       numeric(07),
SYC_TGMONEY                     numeric(07), 
SYC_TGMONEY_TAX                 numeric(07),
SJT_HKNTEN                      numeric(07),
SJT_MONEY                       numeric(07),
SJT_TGMONEY                     numeric(07),
SJT_TGMONEY_TAX                 numeric(07),
KNS_HKNTEN                      numeric(07),
KNS_MONEY                       numeric(07),
KNS_TGMONEY                     numeric(07),
KNS_TGMONEY_TAX                 numeric(07),
GZU_HKNTEN                      numeric(07),
GZU_MONEY                       numeric(07),
GZU_TGMONEY                     numeric(07),
GZU_TGMONEY_TAX                 numeric(07),
ETC_HKNTEN                      numeric(07),
ETC_MONEY                       numeric(07),
ETC_TGMONEY                     numeric(07),
ETC_TGMONEY_TAX                 numeric(07),
NYN_HKNTEN                      numeric(07),
NYN_MONEY                       numeric(07),
NYN_TGMONEY                     numeric(07),
NYN_TGMONEY_TAX                 numeric(07),
TOTAL_HKNTEN                    numeric(07),
TOTAL_MONEY                     numeric(07),
TOTAL_TGMONEY                   numeric(07),
TOTAL_TGMONEY_TAX               numeric(07),
JIHI_1                          numeric(07),
JIHI_1_TAX                      numeric(07),
JIHI_2                          numeric(07),
JIHI_2_TAX                      numeric(07),
JIHI_3                          numeric(07),
JIHI_3_TAX                      numeric(07),
JIHI_4                          numeric(07),
JIHI_4_TAX                      numeric(07),
JIHI_5                          numeric(07),
JIHI_5_TAX                      numeric(07),
JIHI_TOTAL                      numeric(07),
JIHI_TOTAL_TAX                  numeric(07),
JIHI_TAX                        numeric(07),
RJN_FTN                         numeric(07),
KOH_FTN                         numeric(07),
YKZ_FTN                         numeric(07),
RESE_YKZ_FTN                    numeric(07),
KOHTAIYKZKBN                    char(01), 
CHOSEI                          numeric(07),
SECMONEY                        numeric(07),
HKNTEKMONEY                     numeric(07),
DISCOUNT_KBN                    char(02),
DISCOUNT_BODY                   varchar(160),
DISCOUNT_RATEKBN                char(02),
DISCOUNT_TEIRITU                char(01),
DISCOUNT_RATE                   numeric(07),
RSISHOSHIN_MONEY                numeric(07),
RSISAISHIN_MONEY                numeric(07),
RSISDO_MONEY                    numeric(07),
RSIETC_MONEY                    numeric(07),
SKYMONEY                        numeric(07),
NYUKIN_TOTAL                    numeric(07),
NYUKIN_KAISU                    numeric(07),
MISYURIYU                       char(02),
DRCD                            char(04),
NYNSHOKAISU                     numeric(02),
YKZKENNUM                       numeric(05),
RMSAGAKU                        numeric(07),
SHOKUJI1_NISSU                  numeric(02),
SHOKUJI1                        numeric(07),
SHOKUJI2_NISSU                  numeric(02),
SHOKUJI2                        numeric(07),
SHOKUJI3_NISSU                  numeric(02),
SHOKUJI3                        numeric(07),
SHOKUJI4_NISSU                  numeric(02),
SHOKUJI4                        numeric(07),
SHOKUJI5_NISSU                  numeric(02),
SHOKUJI5                        numeric(07),
SHOKUJI6_NISSU                  numeric(02),
SHOKUJI6                        numeric(07),
SHOKUJI7_NISSU                  numeric(02),
SHOKUJI7                        numeric(07),
SYUSKJ_NISSU                    numeric(02),
SYUSKJ_RYOYOHI                  numeric(07),
SYUSKJ_FTN                      numeric(07),
SYUSKJ_FTN_RECE			numeric(07),
SYUSKJ_FTNKBN			char(01),
KOH1SKJ_NISSU                   numeric(02),
KOH1SKJ_RYOYOHI                 numeric(07),
KOH1SKJ_FTN                     numeric(07),
KOH1SKJ_FTN_RECE		numeric(07),
KOH1SKJ_FTNKBN			char(01),
KOH2SKJ_NISSU                   numeric(02),
KOH2SKJ_RYOYOHI                 numeric(07),
KOH2SKJ_FTN                     numeric(07),
KOH2SKJ_FTN_RECE		numeric(07),
KOH2SKJ_FTNKBN			char(01),
KOH3SKJ_NISSU                   numeric(02),
KOH3SKJ_RYOYOHI                 numeric(07),
KOH3SKJ_FTN                     numeric(07),
KOH3SKJ_FTN_RECE		numeric(07),
KOH3SKJ_FTNKBN			char(01),
KOH4SKJ_NISSU                   numeric(02),
KOH4SKJ_RYOYOHI                 numeric(07),
KOH4SKJ_FTN                     numeric(07),
KOH4SKJ_FTN_RECE		numeric(07),
KOH4SKJ_FTNKBN			char(01),
RYOYOHI_SKJ                     numeric(07),
SKYMONEY_SKJ                    numeric(07),
SKYMONEY_SKJ_TAX		numeric(07),
SKYMONEY_SKJ_KEI		numeric(07),
RYOYOHI_SKJ_JIHI		numeric(07),
SKYMONEY_SKJ_JIHI		numeric(07),
SKYMONEY_SKJ_JIHI_TAX		numeric(07),
SKYMONEY_SKJ_JIHI_KEI		numeric(07),
SAIKEISANKBN                    char(01),
INGAISHOHOKBN                   char(01),
DOUJI_DENPNUM                   numeric(07),
CONTKBN                         char(01),
DAY_1                           char(01),
DAY_2                           char(01),
DAY_3                           char(01),
DAY_4                           char(01),
DAY_5                           char(01),
DAY_6                           char(01),
DAY_7                           char(01),
DAY_8                           char(01),
DAY_9                           char(01),
DAY_10                          char(01),
DAY_11                          char(01),
DAY_12                          char(01),
DAY_13                          char(01),
DAY_14                          char(01),
DAY_15                          char(01),
DAY_16                          char(01),
DAY_17                          char(01),
DAY_18                          char(01),
DAY_19                          char(01),
DAY_20                          char(01),
DAY_21                          char(01),
DAY_22                          char(01),
DAY_23                          char(01),
DAY_24                          char(01),
DAY_25                          char(01),
DAY_26                          char(01),
DAY_27                          char(01),
DAY_28                          char(01),
DAY_29                          char(01),
DAY_30                          char(01),
DAY_31                          char(01),
FUKU_DENPNUM			numeric(07),
FUKU_KBN			char(01),
TERMID                          varchar(16),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_SYUNOUWK_primary_key primary key (HOSPID,NYUGAIKBN,PTID,
DENPNUM));


-- TBL_SYUNOU �� TBL_SYUNOUWK   --

insert into TBL_SYUNOUWK
select * from TBL_SYUNOU;

-- TBL_SYUNOU ���               --

drop table TBL_SYUNOU;

-- TBL_SYUNOU ����               --

create table TBL_SYUNOU (
HOSPID				char(24)	not null,
NYUGAIKBN			char(01)	not null,
PTID				numeric(10)     not null,
DENPNUM   			numeric(07)     not null,
DENPLASTNUM  			numeric(02),
SRYKA                           char(02),
SRYYMD                          char(08),
DENPJTIKBN                      char(01),
CREATEKBN                       char(01),
DENPPRTYMD    			char(08),
SKYSTYMD                        char(08),
SKYEDYMD                        char(08),
HKNCOMBINUM                     char(04),
SYUHKNNUM                       char(03),
SYUHKNFTNMONEY                  numeric(07),
SYUCOMPFTN                      numeric(07),
SYUCOMPYKZFTN                   numeric(07),
SYUCOMPTOTAL                    numeric(07),
SYUYKZFTNKBN                    char(01),
KOH1HKNNUM                      char(03),
KOH1HKNFTNMONEY                 numeric(07),
KOH1COMPFTN                     numeric(07),
KOH1COMPYKZFTN                  numeric(07),
KOH1COMPTOTAL                   numeric(07),
KOH1YKZFTNKBN                   char(01),
KOH2HKNNUM                      char(03),
KOH2HKNFTNMONEY                 numeric(07),
KOH2COMPFTN                     numeric(07),
KOH2COMPYKZFTN                  numeric(07),
KOH2COMPTOTAL                   numeric(07),
KOH2YKZFTNKBN                   char(01),
KOH3HKNNUM                      char(03),
KOH3HKNFTNMONEY                 numeric(07),
KOH3COMPFTN                     numeric(07),
KOH3COMPYKZFTN                  numeric(07),
KOH3COMPTOTAL                   numeric(07),
KOH3YKZFTNKBN                   char(01),
KOH4HKNNUM                      char(03),
KOH4HKNFTNMONEY                 numeric(07),
KOH4COMPFTN                     numeric(07),
KOH4COMPYKZFTN 			numeric(07),
KOH4COMPTOTAL                   numeric(07),
KOH4YKZFTNKBN                   char(01),
PTFTNRATE                       numeric(03),
ROOMNUM                         char(08),
BYOTONUM                        char(02),
SKYKUKBN                        char(01),
TAX_TAISHOU                     numeric(07),
TAX_MONEY                       numeric(07),
SKYGK                           numeric(07),
SSU_HKNTEN                      numeric(07),
SSU_MONEY                       numeric(07),
SSU_TGMONEY                     numeric(07),
SSU_TGMONEY_TAX                 numeric(07),
SDO_HKNTEN                      numeric(07),
SDO_MONEY                       numeric(07),
SDO_TGMONEY                     numeric(07),
SDO_TGMONEY_TAX                 numeric(07),
ZTK_HKNTEN                      numeric(07),
ZTK_MONEY                       numeric(07),
ZTK_TGMONEY                     numeric(07),
ZTK_TGMONEY_TAX                 numeric(07),
TYK_HKNTEN                      numeric(07),
TYK_MONEY                       numeric(07),
TYK_TGMONEY                     numeric(07),
TYK_TGMONEY_TAX                 numeric(07),
CSY_HKNTEN                      numeric(07),
CSY_MONEY                       numeric(07),
CSY_TGMONEY                     numeric(07),
CSY_TGMONEY_TAX                 numeric(07),
SYC_HKNTEN                      numeric(07),
SYC_MONEY                       numeric(07),
SYC_TGMONEY                     numeric(07), 
SYC_TGMONEY_TAX                 numeric(07),
SJT_HKNTEN                      numeric(07),
SJT_MONEY                       numeric(07),
SJT_TGMONEY                     numeric(07),
SJT_TGMONEY_TAX                 numeric(07),
KNS_HKNTEN                      numeric(07),
KNS_MONEY                       numeric(07),
KNS_TGMONEY                     numeric(07),
KNS_TGMONEY_TAX                 numeric(07),
GZU_HKNTEN                      numeric(07),
GZU_MONEY                       numeric(07),
GZU_TGMONEY                     numeric(07),
GZU_TGMONEY_TAX                 numeric(07),
ETC_HKNTEN                      numeric(07),
ETC_MONEY                       numeric(07),
ETC_TGMONEY                     numeric(07),
ETC_TGMONEY_TAX                 numeric(07),
NYN_HKNTEN                      numeric(07),
NYN_MONEY                       numeric(07),
NYN_TGMONEY                     numeric(07),
NYN_TGMONEY_TAX                 numeric(07),
TOTAL_HKNTEN                    numeric(07),
TOTAL_MONEY                     numeric(07),
TOTAL_TGMONEY                   numeric(07),
TOTAL_TGMONEY_TAX               numeric(07),
JIHI_1                          numeric(07),
JIHI_1_TAX                      numeric(07),
JIHI_2                          numeric(07),
JIHI_2_TAX                      numeric(07),
JIHI_3                          numeric(07),
JIHI_3_TAX                      numeric(07),
JIHI_4                          numeric(07),
JIHI_4_TAX                      numeric(07),
JIHI_5                          numeric(07),
JIHI_5_TAX                      numeric(07),
JIHI_TOTAL                      numeric(07),
JIHI_TOTAL_TAX                  numeric(07),
JIHI_TAX                        numeric(07),
RJN_FTN                         numeric(07),
KOH_FTN                         numeric(07),
YKZ_FTN                         numeric(07),
RESE_YKZ_FTN                    numeric(07),
KOHTAIYKZKBN                    char(01), 
CHOSEI                          numeric(07),
SECMONEY                        numeric(07),
HKNTEKMONEY                     numeric(07),
DISCOUNT_KBN                    char(02),
DISCOUNT_BODY                   varchar(160),
DISCOUNT_RATEKBN                char(02),
DISCOUNT_TEIRITU                char(01),
DISCOUNT_RATE                   numeric(07),
RSISHOSHIN_MONEY                numeric(07),
RSISAISHIN_MONEY                numeric(07),
RSISDO_MONEY                    numeric(07),
RSIETC_MONEY                    numeric(07),
SKYMONEY                        numeric(07),
NYUKIN_TOTAL                    numeric(07),
NYUKIN_KAISU                    numeric(07),
MISYURIYU                       char(02),
DRCD                            char(04),
NYNSHOKAISU                     numeric(02),
YKZKENNUM                       numeric(05),
RMSAGAKU                        numeric(07),
SHOKUJI1_NISSU                  numeric(02),
SHOKUJI1                        numeric(07),
SHOKUJI2_NISSU                  numeric(02),
SHOKUJI2                        numeric(07),
SHOKUJI3_NISSU                  numeric(02),
SHOKUJI3                        numeric(07),
SHOKUJI4_NISSU                  numeric(02),
SHOKUJI4                        numeric(07),
SHOKUJI5_NISSU                  numeric(02),
SHOKUJI5                        numeric(07),
SHOKUJI6_NISSU                  numeric(02),
SHOKUJI6                        numeric(07),
SHOKUJI7_NISSU                  numeric(02),
SHOKUJI7                        numeric(07),
SYUSKJ_NISSU                    numeric(02),
SYUSKJ_RYOYOHI                  numeric(07),
SYUSKJ_FTN                      numeric(07),
SYUSKJ_FTN_RECE                 numeric(07),
SYUSKJ_FTNKBN                   char(01),
KOH1SKJ_NISSU                   numeric(02),
KOH1SKJ_RYOYOHI                 numeric(07),
KOH1SKJ_FTN                     numeric(07),
KOH1SKJ_FTN_RECE                numeric(07),
KOH1SKJ_FTNKBN                  char(01),
KOH2SKJ_NISSU                   numeric(02),
KOH2SKJ_RYOYOHI                 numeric(07),
KOH2SKJ_FTN                     numeric(07),
KOH2SKJ_FTN_RECE                numeric(07),
KOH2SKJ_FTNKBN                  char(01),
KOH3SKJ_NISSU                   numeric(02),
KOH3SKJ_RYOYOHI                 numeric(07),
KOH3SKJ_FTN                     numeric(07),
KOH3SKJ_FTN_RECE                numeric(07),
KOH3SKJ_FTNKBN                  char(01),
KOH4SKJ_NISSU                   numeric(02),
KOH4SKJ_RYOYOHI                 numeric(07),
KOH4SKJ_FTN                     numeric(07),
KOH4SKJ_FTN_RECE                numeric(07),
KOH4SKJ_FTNKBN                  char(01),
RYOYOHI_SKJ                     numeric(07),
SKYMONEY_SKJ                    numeric(07),
SKYMONEY_SKJ_TAX                numeric(07),
SKYMONEY_SKJ_KEI                numeric(07),
RYOYOHI_SKJ_JIHI                numeric(07),
SKYMONEY_SKJ_JIHI               numeric(07),
SKYMONEY_SKJ_JIHI_TAX           numeric(07),
SKYMONEY_SKJ_JIHI_KEI           numeric(07),
SAIKEISANKBN                    char(01),
INGAISHOHOKBN                   char(01),
DOUJI_DENPNUM                   numeric(07),
CONTKBN                         char(01),
DAY_1                           char(01),
DAY_2                           char(01),
DAY_3                           char(01),
DAY_4                           char(01),
DAY_5                           char(01),
DAY_6                           char(01),
DAY_7                           char(01),
DAY_8                           char(01),
DAY_9                           char(01),
DAY_10                          char(01),
DAY_11                          char(01),
DAY_12                          char(01),
DAY_13                          char(01),
DAY_14                          char(01),
DAY_15                          char(01),
DAY_16                          char(01),
DAY_17                          char(01),
DAY_18                          char(01),
DAY_19                          char(01),
DAY_20                          char(01),
DAY_21                          char(01),
DAY_22                          char(01),
DAY_23                          char(01),
DAY_24                          char(01),
DAY_25                          char(01),
DAY_26                          char(01),
DAY_27                          char(01),
DAY_28                          char(01),
DAY_29                          char(01),
DAY_30                          char(01),
DAY_31                          char(01),
FUKU_DENPNUM			numeric(07),
FUKU_KBN			char(01),
KYUFUGAI_SHOSHIN_TEN		numeric(07)	default 0,
KYUFUGAI_SIDOU_TEN		numeric(07)	default 0,
KYUFUGAI_OSIN1_TEN		numeric(07)	default 0,
KYUFUGAI_OSIN2_TEN		numeric(07)	default 0,
KYUFUGAI_OSIN3_TEN		numeric(07)	default 0,
KYUFUGAI_GOKEI_TEN		numeric(07)	default 0,
TERMID                          varchar(16),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_SYUNOU_primary_key primary key (HOSPID,NYUGAIKBN,PTID,
DENPNUM));


-- TBL_SYUNOUWK �� TBL_SYUNOU   --

insert into TBL_SYUNOU
(HOSPID,
NYUGAIKBN,
PTID,
DENPNUM,
DENPLASTNUM,
SRYKA,
SRYYMD,
DENPJTIKBN,
CREATEKBN,
DENPPRTYMD,
SKYSTYMD,
SKYEDYMD,
HKNCOMBINUM,
SYUHKNNUM,
SYUHKNFTNMONEY,
SYUCOMPFTN,
SYUCOMPYKZFTN,
SYUCOMPTOTAL,
SYUYKZFTNKBN,
KOH1HKNNUM,
KOH1HKNFTNMONEY,
KOH1COMPFTN,
KOH1COMPYKZFTN,
KOH1COMPTOTAL,
KOH1YKZFTNKBN,
KOH2HKNNUM,
KOH2HKNFTNMONEY,
KOH2COMPFTN,
KOH2COMPYKZFTN,
KOH2COMPTOTAL,
KOH2YKZFTNKBN,
KOH3HKNNUM,
KOH3HKNFTNMONEY,
KOH3COMPFTN,
KOH3COMPYKZFTN,
KOH3COMPTOTAL,
KOH3YKZFTNKBN,
KOH4HKNNUM,
KOH4HKNFTNMONEY,
KOH4COMPFTN,
KOH4COMPYKZFTN,
KOH4COMPTOTAL,
KOH4YKZFTNKBN,
PTFTNRATE,
ROOMNUM,
BYOTONUM,
SKYKUKBN,
TAX_TAISHOU,
TAX_MONEY,
SKYGK,
SSU_HKNTEN,
SSU_MONEY,
SSU_TGMONEY,
SSU_TGMONEY_TAX,
SDO_HKNTEN,
SDO_MONEY,
SDO_TGMONEY,
SDO_TGMONEY_TAX,
ZTK_HKNTEN,
ZTK_MONEY,
ZTK_TGMONEY,
ZTK_TGMONEY_TAX,
TYK_HKNTEN,
TYK_MONEY,
TYK_TGMONEY,
TYK_TGMONEY_TAX,
CSY_HKNTEN,
CSY_MONEY,
CSY_TGMONEY,
CSY_TGMONEY_TAX,
SYC_HKNTEN,
SYC_MONEY,
SYC_TGMONEY,
SYC_TGMONEY_TAX,
SJT_HKNTEN,
SJT_MONEY,
SJT_TGMONEY,
SJT_TGMONEY_TAX,
KNS_HKNTEN,
KNS_MONEY,
KNS_TGMONEY,
KNS_TGMONEY_TAX,
GZU_HKNTEN,
GZU_MONEY,
GZU_TGMONEY,
GZU_TGMONEY_TAX,
ETC_HKNTEN,
ETC_MONEY,
ETC_TGMONEY,
ETC_TGMONEY_TAX,
NYN_HKNTEN,
NYN_MONEY,
NYN_TGMONEY,
NYN_TGMONEY_TAX,
TOTAL_HKNTEN,
TOTAL_MONEY,
TOTAL_TGMONEY,
TOTAL_TGMONEY_TAX,
JIHI_1,
JIHI_1_TAX,
JIHI_2,
JIHI_2_TAX,
JIHI_3,
JIHI_3_TAX,
JIHI_4,
JIHI_4_TAX,
JIHI_5,
JIHI_5_TAX,
JIHI_TOTAL,
JIHI_TOTAL_TAX,
JIHI_TAX,
RJN_FTN,
KOH_FTN,
YKZ_FTN,
RESE_YKZ_FTN,
KOHTAIYKZKBN,
CHOSEI,
SECMONEY,
HKNTEKMONEY,
DISCOUNT_KBN,
DISCOUNT_BODY,
DISCOUNT_RATEKBN,
DISCOUNT_TEIRITU,
DISCOUNT_RATE,
RSISHOSHIN_MONEY,
RSISAISHIN_MONEY,
RSISDO_MONEY,
RSIETC_MONEY,
SKYMONEY,
NYUKIN_TOTAL,
NYUKIN_KAISU,
MISYURIYU,
DRCD,
NYNSHOKAISU,
YKZKENNUM,
RMSAGAKU,
SHOKUJI1_NISSU,
SHOKUJI1,
SHOKUJI2_NISSU,
SHOKUJI2,
SHOKUJI3_NISSU,
SHOKUJI3,
SHOKUJI4_NISSU,
SHOKUJI4,
SHOKUJI5_NISSU,
SHOKUJI5,
SHOKUJI6_NISSU,
SHOKUJI6,
SHOKUJI7_NISSU,
SHOKUJI7,
SYUSKJ_NISSU,
SYUSKJ_RYOYOHI,
SYUSKJ_FTN,
SYUSKJ_FTN_RECE,
SYUSKJ_FTNKBN,
KOH1SKJ_NISSU,
KOH1SKJ_RYOYOHI,
KOH1SKJ_FTN,
KOH1SKJ_FTN_RECE,
KOH1SKJ_FTNKBN,
KOH2SKJ_NISSU,
KOH2SKJ_RYOYOHI,
KOH2SKJ_FTN,
KOH2SKJ_FTN_RECE,
KOH2SKJ_FTNKBN,
KOH3SKJ_NISSU,
KOH3SKJ_RYOYOHI,
KOH3SKJ_FTN,
KOH3SKJ_FTN_RECE,
KOH3SKJ_FTNKBN,
KOH4SKJ_NISSU,
KOH4SKJ_RYOYOHI,
KOH4SKJ_FTN,
KOH4SKJ_FTN_RECE,
KOH4SKJ_FTNKBN,
RYOYOHI_SKJ,
SKYMONEY_SKJ,
SKYMONEY_SKJ_TAX,
SKYMONEY_SKJ_KEI,
RYOYOHI_SKJ_JIHI,
SKYMONEY_SKJ_JIHI,
SKYMONEY_SKJ_JIHI_TAX,
SKYMONEY_SKJ_JIHI_KEI,
SAIKEISANKBN,
INGAISHOHOKBN,
DOUJI_DENPNUM,
CONTKBN,
DAY_1,
DAY_2,
DAY_3,
DAY_4,
DAY_5,
DAY_6,
DAY_7,
DAY_8,
DAY_9,
DAY_10,
DAY_11,
DAY_12,
DAY_13,
DAY_14,
DAY_15,
DAY_16,
DAY_17,
DAY_18,
DAY_19,
DAY_20,
DAY_21,
DAY_22,
DAY_23,
DAY_24,
DAY_25,
DAY_26,
DAY_27,
DAY_28,
DAY_29,
DAY_30,
DAY_31,
FUKU_DENPNUM,
FUKU_KBN,
--KYUFUGAI_SHOSHIN_TEN--
--KYUFUGAI_SIDOU_TEN--
--KYUFUGAI_OSIN1_TEN--
--KYUFUGAI_OSIN2_TEN--
--KYUFUGAI_OSIN3_TEN--
--KYUFUGAI_GOKEI_TEN--
TERMID,
OPID,
CREYMD,
UPYMD,
UPHMS
)
select HOSPID,NYUGAIKBN,PTID,DENPNUM,DENPLASTNUM,SRYKA,SRYYMD,
DENPJTIKBN,CREATEKBN,DENPPRTYMD,SKYSTYMD,SKYEDYMD,HKNCOMBINUM,SYUHKNNUM,
SYUHKNFTNMONEY,SYUCOMPFTN,SYUCOMPYKZFTN,SYUCOMPTOTAL,
SYUYKZFTNKBN,KOH1HKNNUM,KOH1HKNFTNMONEY,KOH1COMPFTN,KOH1COMPYKZFTN,
KOH1COMPTOTAL,KOH1YKZFTNKBN,KOH2HKNNUM,KOH2HKNFTNMONEY,KOH2COMPFTN,
KOH2COMPYKZFTN,KOH2COMPTOTAL,KOH2YKZFTNKBN,KOH3HKNNUM,KOH3HKNFTNMONEY,
KOH3COMPFTN,KOH3COMPYKZFTN,KOH3COMPTOTAL,KOH3YKZFTNKBN,KOH4HKNNUM,
KOH4HKNFTNMONEY,KOH4COMPFTN,KOH4COMPYKZFTN,KOH4COMPTOTAL,KOH4YKZFTNKBN,
PTFTNRATE,ROOMNUM,BYOTONUM,SKYKUKBN,TAX_TAISHOU,TAX_MONEY,SKYGK,
SSU_HKNTEN,SSU_MONEY,SSU_TGMONEY,SSU_TGMONEY_TAX,SDO_HKNTEN,
SDO_MONEY,SDO_TGMONEY,SDO_TGMONEY_TAX,ZTK_HKNTEN,ZTK_MONEY,
ZTK_TGMONEY,ZTK_TGMONEY_TAX,TYK_HKNTEN,TYK_MONEY,TYK_TGMONEY,
TYK_TGMONEY_TAX,CSY_HKNTEN,CSY_MONEY,CSY_TGMONEY,CSY_TGMONEY_TAX,
SYC_HKNTEN,SYC_MONEY,SYC_TGMONEY,SYC_TGMONEY_TAX,SJT_HKNTEN,
SJT_MONEY,SJT_TGMONEY,SJT_TGMONEY_TAX,KNS_HKNTEN,KNS_MONEY,
KNS_TGMONEY,KNS_TGMONEY_TAX,GZU_HKNTEN,GZU_MONEY,GZU_TGMONEY,
GZU_TGMONEY_TAX,ETC_HKNTEN,ETC_MONEY,ETC_TGMONEY,ETC_TGMONEY_TAX,
NYN_HKNTEN,NYN_MONEY,NYN_TGMONEY,NYN_TGMONEY_TAX,TOTAL_HKNTEN,
TOTAL_MONEY,TOTAL_TGMONEY,TOTAL_TGMONEY_TAX,JIHI_1,JIHI_1_TAX,
JIHI_2,JIHI_2_TAX,JIHI_3,JIHI_3_TAX,JIHI_4,JIHI_4_TAX,
JIHI_5,JIHI_5_TAX,JIHI_TOTAL,JIHI_TOTAL_TAX,JIHI_TAX,RJN_FTN,
KOH_FTN,YKZ_FTN,RESE_YKZ_FTN,KOHTAIYKZKBN,CHOSEI,SECMONEY,
HKNTEKMONEY,DISCOUNT_KBN,DISCOUNT_BODY,DISCOUNT_RATEKBN,
DISCOUNT_TEIRITU,DISCOUNT_RATE,RSISHOSHIN_MONEY,RSISAISHIN_MONEY,
RSISDO_MONEY,RSIETC_MONEY,SKYMONEY,NYUKIN_TOTAL,NYUKIN_KAISU,
MISYURIYU,DRCD,NYNSHOKAISU,YKZKENNUM,RMSAGAKU,
SHOKUJI1_NISSU,SHOKUJI1,
SHOKUJI2_NISSU,SHOKUJI2,
SHOKUJI3_NISSU,SHOKUJI3,
SHOKUJI4_NISSU,SHOKUJI4,
SHOKUJI5_NISSU,SHOKUJI5,
SHOKUJI6_NISSU,SHOKUJI6,
SHOKUJI7_NISSU,SHOKUJI7,
SYUSKJ_NISSU,SYUSKJ_RYOYOHI,SYUSKJ_FTN,
SYUSKJ_FTN_RECE,
SYUSKJ_FTNKBN,
KOH1SKJ_NISSU,KOH1SKJ_RYOYOHI,KOH1SKJ_FTN,
KOH1SKJ_FTN_RECE,
KOH1SKJ_FTNKBN,
KOH2SKJ_NISSU,KOH2SKJ_RYOYOHI,KOH2SKJ_FTN,
KOH2SKJ_FTN_RECE,
KOH2SKJ_FTNKBN,
KOH3SKJ_NISSU,KOH3SKJ_RYOYOHI,KOH3SKJ_FTN,
KOH3SKJ_FTN_RECE,
KOH3SKJ_FTNKBN,
KOH4SKJ_NISSU,KOH4SKJ_RYOYOHI,KOH4SKJ_FTN,
KOH4SKJ_FTN_RECE,
KOH4SKJ_FTNKBN,
RYOYOHI_SKJ,SKYMONEY_SKJ,
SKYMONEY_SKJ_TAX,
SKYMONEY_SKJ_KEI,
RYOYOHI_SKJ_JIHI,
SKYMONEY_SKJ_JIHI,
SKYMONEY_SKJ_JIHI_TAX,
SKYMONEY_SKJ_JIHI_KEI,
SAIKEISANKBN,INGAISHOHOKBN,
DOUJI_DENPNUM,CONTKBN,
DAY_1,
DAY_2,
DAY_3,
DAY_4,
DAY_5,
DAY_6,
DAY_7,
DAY_8,
DAY_9,
DAY_10,
DAY_11,
DAY_12,
DAY_13,
DAY_14,
DAY_15,
DAY_16,
DAY_17,
DAY_18,
DAY_19,
DAY_20,
DAY_21,
DAY_22,
DAY_23,
DAY_24,
DAY_25,
DAY_26,
DAY_27,
DAY_28,
DAY_29,
DAY_30,
DAY_31,
FUKU_DENPNUM,
FUKU_KBN,
TERMID,OPID,CREYMD,UPYMD,UPHMS from TBL_SYUNOUWK;

-- TBL_SYUNOUWK ���             --

drop table TBL_SYUNOUWK;  

