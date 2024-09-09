\set ON_ERROR_STOP
--
-- オンライン返戻明細テーブル
--
-- Create Date : 2008/12/12         --
--
create	table	tbl_henrei_body	(
	HOSPNUM			smallint,
	TEISYUTUSAKI		char(1),
	SYORIYM			char(6),
	RECEIPT_NO		char(6),
	PT_RENNUM		smallint,
	SELECT_FLG		smallint,
	RECESYUBETU		char(4),
	NYUGAIKBN		char(1),
	SRYYM			char(6),
	NAME			varchar(40),
	SEX			char(1),
	BIRTHDAY		char(8),
	PTNUM			char(20),
	PTID			bigint,
	HKNJANUM		char(8),
	KIGO			varchar(40),
	NUM			varchar(40),
	FTNNUM_1		varchar(8),
	JKYNUM_1		varchar(7),
	FTNNUM_2		varchar(8),
	JKYNUM_2		varchar(7),
	FTNNUM_3		varchar(8),
	JKYNUM_3		varchar(7),
	FTNNUM_4		varchar(8),
	JKYNUM_4		varchar(7),
	SEARCH_NO		varchar(30),
	CHECK_NO		varchar(100),
	HR_CODE			char(5),
	HR_MSG			varchar(1000),
	HENREI_INFO1		varchar(20),
	HENREI_INFO2		varchar(2200),
	ERRCD			char(3),
	S_SYORIYMD_1		char(8),
	S_MAKE_FLG_1		smallint,
	S_HENREIYM_1		char(6),
	S_HENREI_SIJI_FLG_1     smallint,
	S_TEISYUTUSAKI_1	char(1),
	S_NYUGAIKBN_1		char(1),
	S_RECEKA_1		char(2),
	S_RECESYUBETU_1		char(4),
	S_HKNJANUM_1		char(8),
	S_HOJOKBN_KEY_1		char(1),
	S_TEKSTYMD_1		char(8),
	S_KIGO_1		varchar(40),
	S_SYORIYMD_2		char(8),
	S_MAKE_FLG_2		smallint,
	S_HENREIYM_2		char(6),
	S_HENREI_SIJI_FLG_2     smallint,
	S_TEISYUTUSAKI_2	char(1),
	S_NYUGAIKBN_2		char(1),
	S_RECEKA_2		char(2),
	S_RECESYUBETU_2		char(4),
	S_HKNJANUM_2		char(8),
	S_HOJOKBN_KEY_2		char(1),
	S_TEKSTYMD_2		char(8),
	S_KIGO_2		varchar(40),
	S_SYORIYMD_3		char(8),
	S_MAKE_FLG_3		smallint,
	S_HENREIYM_3		char(6),
	S_HENREI_SIJI_FLG_3     smallint,
	S_TEISYUTUSAKI_3	char(1),
	S_NYUGAIKBN_3		char(1),
	S_RECEKA_3		char(2),
	S_RECESYUBETU_3		char(4),
	S_HKNJANUM_3		char(8),
	S_HOJOKBN_KEY_3		char(1),
	S_TEKSTYMD_3		char(8),
	S_KIGO_3		varchar(40),
	S_SYORIYMD_4		char(8),
	S_MAKE_FLG_4		smallint,
	S_HENREIYM_4		char(6),
	S_HENREI_SIJI_FLG_4     smallint,
	S_TEISYUTUSAKI_4	char(1),
	S_NYUGAIKBN_4		char(1),
	S_RECEKA_4		char(2),
	S_RECESYUBETU_4		char(4),
	S_HKNJANUM_4		char(8),
	S_HOJOKBN_KEY_4		char(1),
	S_TEKSTYMD_4		char(8),
	S_KIGO_4		varchar(40),
	S_SYORIYMD_5		char(8),
	S_MAKE_FLG_5		smallint,
	S_HENREIYM_5		char(6),
	S_HENREI_SIJI_FLG_5     smallint,
	S_TEISYUTUSAKI_5	char(1),
	S_NYUGAIKBN_5		char(1),
	S_RECEKA_5		char(2),
	S_RECESYUBETU_5		char(4),
	S_HKNJANUM_5		char(8),
	S_HOJOKBN_KEY_5		char(1),
	S_TEKSTYMD_5		char(8),
	S_KIGO_5		varchar(40),
	S_SYORIYMD_6		char(8),
	S_MAKE_FLG_6		smallint,
	S_HENREIYM_6		char(6),
	S_HENREI_SIJI_FLG_6     smallint,
	S_TEISYUTUSAKI_6	char(1),
	S_NYUGAIKBN_6		char(1),
	S_RECEKA_6		char(2),
	S_RECESYUBETU_6		char(4),
	S_HKNJANUM_6		char(8),
	S_HOJOKBN_KEY_6		char(1),
	S_TEKSTYMD_6		char(8),
	S_KIGO_6		varchar(40),
	S_SYORIYMD_7		char(8),
	S_MAKE_FLG_7		smallint,
	S_HENREIYM_7		char(6),
	S_HENREI_SIJI_FLG_7     smallint,
	S_TEISYUTUSAKI_7	char(1),
	S_NYUGAIKBN_7		char(1),
	S_RECEKA_7		char(2),
	S_RECESYUBETU_7		char(4),
	S_HKNJANUM_7		char(8),
	S_HOJOKBN_KEY_7		char(1),
	S_TEKSTYMD_7		char(8),
	S_KIGO_7		varchar(40),
	S_SYORIYMD_8		char(8),
	S_MAKE_FLG_8		smallint,
	S_HENREIYM_8		char(6),
	S_HENREI_SIJI_FLG_8     smallint,
	S_TEISYUTUSAKI_8	char(1),
	S_NYUGAIKBN_8		char(1),
	S_RECEKA_8		char(2),
	S_RECESYUBETU_8		char(4),
	S_HKNJANUM_8		char(8),
	S_HOJOKBN_KEY_8		char(1),
	S_TEKSTYMD_8		char(8),
	S_KIGO_8		varchar(40),
	S_SYORIYMD_9		char(8),
	S_MAKE_FLG_9		smallint,
	S_HENREIYM_9		char(6),
	S_HENREI_SIJI_FLG_9     smallint,
	S_TEISYUTUSAKI_9	char(1),
	S_NYUGAIKBN_9		char(1),
	S_RECEKA_9		char(2),
	S_RECESYUBETU_9		char(4),
	S_HKNJANUM_9		char(8),
	S_HOJOKBN_KEY_9		char(1),
	S_TEKSTYMD_9		char(8),
	S_KIGO_9		varchar(40),
	S_SYORIYMD_10		char(8),
	S_MAKE_FLG_10		smallint,
	S_HENREIYM_10		char(6),
	S_HENREI_SIJI_FLG_10    smallint,
	S_TEISYUTUSAKI_10	char(1),
	S_NYUGAIKBN_10		char(1),
	S_RECEKA_10		char(2),
	S_RECESYUBETU_10	char(4),
	S_HKNJANUM_10		char(8),
	S_HOJOKBN_KEY_10	char(1),
	S_TEKSTYMD_10		char(8),
	S_KIGO_10		varchar(40),
	TERMID			varchar(64),
	OPID			varchar(16),
	CREYMD			char(8),
	UPYMD			char(8),
	UPHMS			char(6),
	Constraint "tbl_henrei_body_primary_key" primary key(
                HOSPNUM,
                TEISYUTUSAKI,
                SYORIYM,
                RECEIPT_NO,
                PT_RENNUM
        )
);


CREATE INDEX idx_henrei_body_kanriinfo ON tbl_henrei_body USING btree (hospnum, check_no);
CREATE INDEX idx_henrei_body_ptid ON tbl_henrei_body USING btree (hospnum, sryym, ptid);
