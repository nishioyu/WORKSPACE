\set ON_ERROR_STOP

delete from tbl_chk004;

COPY "tbl_chk004" FROM stdin;
1102	000001  	20040401	99999999	        00020311			20060609	20060609	183108
1102	000002  	20040401	99999999	        00020321			20060609	20060609	183112
1102	000003  	20040401	99999999	        00020341			20060609	20060609	183116
1102	000004  	20040401	99999999	        00020601			20060609	20060609	172252
1102	000005  	20040401	99999999	        00020671			20060609	20060609	172258
\.
