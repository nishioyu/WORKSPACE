ALTER TABLE ONLY tbl_generic_log
  DROP CONSTRAINT tbl_generic_log_key;
ALTER TABLE ONLY tbl_generic_log
  ADD CONSTRAINT tbl_generic_log_primary_key PRIMARY KEY (HOSPNUM,PTID,SRYYMD,PRINTYMD,PRINTHMS,RECIPENUM);