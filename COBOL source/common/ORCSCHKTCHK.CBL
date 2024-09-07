      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSCHKTCHK.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¿ÇÎÅ¹Ô°ÙÆþÎÏ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Êñ³ç¿ÇÎÅ¹Ô°Ù¥Á¥§¥Ã¥¯½èÍý
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  07/02/XX    NACL¡ÝÂ¿¡¹Ç¼  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  03.05.00    NACL-Â¿¡¹Ç¼  07/04/XX  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  04.04.00    NACL-Â¿¡¹Ç¼  08/12/XX  Æþ±¡ÎÁÊñ³çÂÐ±þ
      *  04.05.00    NACL-Â¿¡¹Ç¼  09/09/XX  ³°Íè¤Þ¤È¤áÂÐ±þ
      *                                     ËýÀ­°Ý»ýÆ©ÀÏ°å³Ø´ÉÍýÎÁÅùÂÐ±þ
      *  04.05.00    NACL-Â¿¡¹Ç¼  10/02/XX  ¥¢¥Õ¥¿¡¼¥±¥¢ÂÐ¾Ý³°ÂÐ±þ
      *  04.06.00    NACL-Â¿¡¹Ç¼  10/10/XX  Êñ³ç»»Äê¡ÊºÞ¡ËÂÐ±þ
      *  04.06.00    NACL-Â¿¡¹Ç¼  11/01/XX  ÅÅ»ÒÅÀ¿ôÉ½¥Þ¥¹¥¿ÂÐ±þ
      *  04.06.00    NACL-Â¿¡¹Ç¼  12/03/28  £È£²£´Ç¯£´·î²þÀµÂÐ±þ
      *  04.07.00    NACL-Â¿¡¹Ç¼  12/08/XX  ¥µ¥ó¥×¥ëÌô¡Ê¥µ¡ËÂÐ±þ
      *  04.07.00    NACL-Â¿¡¹Ç¼  14/03/XX  £È£²£¶Ç¯£´·î²þÀµÂÐ±þ
      *                                     Ã»´üÂÚºß¼ê½ÑÂÐ±þ
      *  04.08.00    NACL-Â¿¡¹Ç¼  15/09/XX  Ã»´üÂÚºß¼ê½Ñ£³¡¡³°ÍèÂÐ±þ
      *  04.08.00    NACL-Â¿¡¹Ç¼  16/03/XX  £È£²£¸Ç¯£´·î²þÀµÂÐ±þ
      *                                     ÂÐ¾Ý¥Æ¡¼¥Ö¥ë£²£°£°¤Ø
      *  04.08.00    NACL-Â¿¡¹Ç¼  17/01/17  Íâ·îÆþ±¡ÎÁ¥Á¥§¥Ã¥¯ÂÐ±þ
      *  04.08.00    NACL-Â¿¡¹Ç¼  17/04/XX  Ã»½Ñ£³¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»ÂÐ±þ
      *  04.08.00    NACL-Â¿¡¹Ç¼  17/09/XX  Åö·î¼õ¿Ç¤¢¤ê¥Á¥§¥Ã¥¯ÂÐ±þ
      *  05.00.00    NACL-Â¿¡¹Ç¼  18/10/XX  Ã»½Ñ£³²èÁü¿ÇÃÇÅù¥Á¥§¥Ã¥¯ÂÐ±þ
      *  05.00.00    NACL-Â¿¡¹Ç¼  20/10/XX  £Ð£Ã£Ò¸¡ºº¤ÎÈ½ÃÇÎÁ½ÐÍè¹âÂÐ±þ
      *  05.00.00    NMED-Â¿¡¹Ç¼  20/12/XX  £Ð£Ã£Ò¸¡ººÄÉ²ÃÂÐ±þ
      *  05.00.00    ORCAMO       21/04/14  Ã»¼ê£³Êñ³ç»»Äê¤Ê¤·¤Î»»ÄêÆüÂÐ±þ
      *  05.01.00    ORCAMO       22/07/XX  £Ð£Ã£Ò¸¡ººÄÉ²ÃÂÐ±þ
      *  05.01.00    ORCAMO       22/09/XX  £Ð£Ã£Ò¸¡ººÄÉ²ÃÂÐ±þ
      *  05.01.00    ORCAMO       22/11/XX  £Ð£Ã£Ò¸¡ººÄÉ²ÃÂÐ±þ
      *  05.02.00    ORCAMO       24/04/XX  ÂÐ¾Ý¥Æ¡¼¥Ö¥ë400¤Ø¡ÊR6.4ÂÐ±þ¡Ë
      *                                     Êñ³ç¥¨¥é¡¼ºï½üÂÐ±þ
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-HKTSANTEI           PIC 9(01).
           03  FLG-HKTSRYCD            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SANTEI              PIC 9(01).
           03  FLG-NYUINACT            PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
      *
           03  FLG-CHK                 PIC 9(01).
           03  FLG-CHK2                PIC 9(01).
           03  FLG-CHK3                PIC 9(01).
           03  FLG-CHK4                PIC 9(01).
           03  FLG-HOUKATU             PIC 9(01).
           03  FLG-SONOTA              PIC 9(01).
           03  FLG-ZAIARI              PIC 9(01).
           03  FLG-SANTEIOK            PIC 9(01).
           03  FLG-INPUTOK             PIC 9(01).
           03  FLG-DEKIDAKA            PIC 9(01).
      *
           03  FLG-HKTINP              PIC 9(01).
      *ver.4.7
           03  FLG-INPUTOK2            PIC 9(01).
           03  FLG-INPUTOK6            PIC 9(01).
           03  FLG-INPUTOK7            PIC 9(01).
      *H29.4
           03  FLG-TANKI3HKT-OK        PIC 9(01).
           03  FLG-TANKI3HKT-NO        PIC 9(01).
      *H29.9
           03  FLG-TEISEI-ARI          PIC 9(01).
           03  FLG-JYURRK-CHK          PIC 9(01).
      *R03.4
           03  FLG-TANKI2              PIC 9(01).
      *
      *2024/04
           03  FLG-ERRARI              PIC 9(01).
           03  FLG-ERRGAI              PIC 9(01).
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX-N                   PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDX-S                   PIC 9(04).
           03  IDX-H                   PIC 9(04).
           03  IDX-K                   PIC 9(04).
           03  IDX-Z                   PIC 9(04).
           03  IDX-Z2                  PIC 9(04).
           03  IDX-Z3                  PIC 9(04).
      *
           03  IDX-L1                  PIC 9(04).
           03  IDX-L2                  PIC 9(04).
           03  IDX-ERR                 PIC 9(04).
      *
           03  IDD                     PIC 9(02).
           03  IDD2                    PIC 9(02).
           03  IDX-D                   PIC 9(02).
           03  IDX-SN                  PIC 9(04).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-TEI-CNT             PIC 9(03).
           03  WRK-DAY                 PIC S9(03).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
           03  WRK-ERRDAY              PIC 9(02).
      *
           03  WRK-K-SRYCD             PIC X(09).
           03  WRK-NT-SRYCD            PIC X(09).
      *
           03  WRK-IDX-N               PIC 9(04).
      *H27.9
           03  WRK-TANKI3-IDX          PIC 9(04).
      *
      *    Êñ³ç¤¹¤ë¿ÇÎÅ¶èÊ¬
           03  WRK-HKT-SRYKBN-G.
               05  WRK-HKTKBN20        PIC 9(01).
               05  WRK-HKTKBN30        PIC 9(01).
               05  WRK-HKTKBN40        PIC 9(01).
               05  WRK-HKTKBN50        PIC 9(01).
               05  WRK-HKTKBN60        PIC 9(01).
               05  WRK-HKTKBN70        PIC 9(01).
               05  WRK-HKTKBN80        PIC 9(01).
               05  WRK-HKTKBN81        PIC 9(01).
               05  WRK-HKTKBN82        PIC 9(01).
               05  WRK-HKTKBN83        PIC 9(01).
               05  WRK-HKTKBN64        PIC 9(01).
               05  WRK-HKTKBN13        PIC 9(01).
               05  WRK-HKTKBN14        PIC 9(01).
               05  WRK-HKTKBN10        PIC 9(01).
      *    Êñ³ç
           03  WRK-NYUHKT-AREA.
               05  WRK-NYUHKT-OCCURS   OCCURS   31.
                   07  WRK-NYUHKTCD        PIC X(03).
                   07  WRK-N-SRYCD         PIC X(09).
      *
      *    ¼«Æ°È¯À¸¤Î¥Á¥§¥Ã¥¯¶èÊ¬
           03  WRK-ORCSC-KASAN.
               05  WRK-ORCSC-KASAN-TBL  OCCURS  10.
                   07  WRK-ORCSC-KASN-TBL2     OCCURS   30.
      *                Êñ³çºÞ
                       09  WRK-ORCSC-HKTKBN        PIC 9(01).
      *H29.1
      *    Íâ·îÆþ±¡ÎÁ
           03  WRK-N-SRYYM.
               05  WRK-N-SRYYY         PIC 9(04).
               05  WRK-N-SRYMM         PIC 9(02).
           03  WRK-TANKI-DAY           PIC 9(02).
           03  WRK-LST-DD              PIC 9(02).
      *
      *H30.10
           03  WRK-CHK-SRYKBN          PIC X(02).
      *
      *R02.10
           03  FLG-SARSCOV          PIC 9(01).
           03  FLG-SANTEINO         PIC 9(01).
           03  WRK-COV-HANDAN1      PIC X(09).
           03  WRK-COV-HANDAN2      PIC X(09).
      *2024/04
           03  WRK-G-SRYSYUKBN       PIC X(03).
      *
      *    ¥Á¥§¥Ã¥¯ÍÑ
       01  WRK-TBL-AREA.
           03  WRK-KANRICD             PIC  X(03).
           03  WRK-SANTEI-HOU          PIC  X(01).
           03  WRK-SANTEI-KBN          PIC  X(01).
           03  WRK-NYUGAIKBN           PIC  X(01).
           03  WRK-HKNCOMBIKBN         PIC  X(01).
           03  WRK-SRYKBN-G.
               07  WRK-SRYKBN-KBN      PIC X(01)
                                           OCCURS   50.
      *
       01  TBL-AREA.
           03  TBL-DATA-G              OCCURS   100.
               05  TBL-KANRICD             PIC  X(03).
               05  TBL-SANTEI-HOU          PIC  X(01).
               05  TBL-SANTEI-KBN          PIC  X(01).
               05  TBL-NYUGAIKBN           PIC  X(01).
               05  TBL-HKNCOMBIKBN         PIC  X(01).
               05  TBL-SRYKBN-G.
                   07  TBL-SRYKBN-KBN      PIC X(01)
                                           OCCURS   50.
               05  TBL-SRYCD-G.
                   07  TBL-SRYCD           PIC X(09)
                                           OCCURS   400.
           03  TBL-MAX                 PIC 9(04).
      *Æþ±¡ÎÁ
       01  TBL-NYUAREA.
           03  TBL-NYUDATA-G              OCCURS   100.
               05  TBL-N-KANRICD             PIC  X(03).
               05  TBL-N-SANTEI-HOU          PIC  X(01).
               05  TBL-N-SANTEI-KBN          PIC  X(01).
               05  TBL-N-NYUGAIKBN           PIC  X(01).
               05  TBL-N-HKNCOMBIKBN         PIC  X(01).
               05  TBL-N-SRYKBN-G.
                   07  TBL-N-SRYKBN-KBN      PIC X(01)
                                             OCCURS   50.
               05  TBL-N-SRYCD-G.
                   07  TBL-N-SRYCD           PIC X(09)
                                             OCCURS   400.
           03  TBL-N-MAX                 PIC 9(04).
      *
       01  TBL-HKTSANTEI-AREA.
           03  TBL-HKTSANTEI-DATA-G        OCCURS   600.
               05  TBL-HKTSRYCD            PIC X(09).
               05  TBL-SANTEIKBN           PIC X(01).
               05  TBL-KENTAIKBN           PIC X(01).
      *
      *    ½ÐÍè¹â»»ÄêÆü
       01  TBL-DEKIDAKA-AREA.
           03  TBL-DEKIDAKA-G              OCCURS   31.
               05  TBL-DEKIDAKA            PIC 9(01).
      *
      *    ÊÝÂ¸£Ó£Ð£ÁÎÎ°è¥ï¡¼¥¯
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *
      *H28.9
      *»ý»²ÌôÍ½Ìó¥³¡¼¥É
       01  CONS-AREA.
           03  CONS-JSNYCD1        PIC X(09)    VALUE   "099209811".
           03  CONS-JSNYCD2        PIC X(09)    VALUE   "099209812".
           03  CONS-JSNYCD3        PIC X(09)    VALUE   "099209821".
           03  CONS-JSNYCD4        PIC X(09)    VALUE   "099209822".
      *
           03  CONS-JSNYCD5        PIC X(09)    VALUE   "099209711".
           03  CONS-JSNYCD6        PIC X(09)    VALUE   "099209712".
           03  CONS-JSNYCD7        PIC X(09)    VALUE   "099209721".
           03  CONS-JSNYCD8        PIC X(09)    VALUE   "099209722".
      *
      *R02.10
      *    £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²³Ë»À¸¡½Ð¡Ê¸¡ºº°ÑÂ÷¡Ë
           03  CONS-COV21          PIC X(09)    VALUE   "160223350".
      *    £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²³Ë»À¸¡½Ð¡Ê¸¡ºº°ÑÂ÷°Ê³°¡Ë
           03  CONS-COV22          PIC X(09)    VALUE   "160223450".
      *    ¥¦¥¤¥ë¥¹¡¦ºÙ¶Ý³Ë»ÀÂ¿¹àÌÜÆ±»þ¸¡½Ð¡Ê¸¡ºº°ÑÂ÷¡Ë
           03  CONS-COV23          PIC X(09)    VALUE   "160224050".
      *    ¥¦¥¤¥ë¥¹¡¦ºÙ¶Ý³Ë»ÀÂ¿¹àÌÜÆ±»þ¸¡½Ð¡Ê¸¡ºº°ÑÂ÷°Ê³°¡Ë
           03  CONS-COV24          PIC X(09)    VALUE   "160224150".
      *
      *    £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²¹³¸¶¸¡½Ð
           03  CONS-COV11          PIC X(09)    VALUE   "160223550".
      *    £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²¹³¸¶¸¡½Ð¡ÊÄêÎÌ¡Ë
           03  CONS-COV12          PIC X(09)    VALUE   "160224250".
      *R03.5
      *    £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²¡¦¥¤¥ó¥Õ¥ë¥¨¥ó¥¶¹³¸¶Æ±»þ¸¡½Ð
           03  CONS-COV13          PIC X(09)    VALUE   "160226450".
      *R02.12
      *    £Ó£Á£Ò£Ó¡¼£Ã£ï£Ö¡¼£²¡¦¥¤¥ó¥Õ¥ë¥¨¥ó¥¶³Ë»ÀÆ±»þ¸¡½Ð¡Ê¸¡ºº°ÑÂ÷¡Ë
           03  CONS-COV25          PIC X(09)    VALUE   "160224750".
      *    £Ó£Á£Ò£Ó¡¼£Ã£ï£Ö¡¼£²¡¦¥¤¥ó¥Õ¥ë¥¨¥ó¥¶³Ë»ÀÆ±»þ¸¡½Ð¡Ê¸¡ºº°ÑÂ÷°Ê³°¡Ë
           03  CONS-COV26          PIC X(09)    VALUE   "160224850".
      *
      *R03.12
      *  2021.12.31 ¤«¤é³«»Ï
      *    £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²³Ë»À¸¡½Ð¡Ê¸¡ºº°ÑÂ÷¡Ë
           03  CONS-COV212          PIC X(09)    VALUE   "160229450".
      *    £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²³Ë»À¸¡½Ð¡Ê¸¡ºº°ÑÂ÷°Ê³°¡Ë
           03  CONS-COV222          PIC X(09)    VALUE   "160229550".
      *    £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²¹³¸¶¸¡½Ð¡ÊÄêÀ­¡Ë
           03  CONS-COV112          PIC X(09)    VALUE   "160229850".
      *    £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²¹³¸¶¸¡½Ð¡ÊÄêÎÌ¡Ë
           03  CONS-COV122          PIC X(09)    VALUE   "160229950".
      *    £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²¡¦¥¤¥ó¥Õ¥ë¥¨¥ó¥¶¹³¸¶Æ±»þ¸¡½Ð
           03  CONS-COV132          PIC X(09)    VALUE   "160230050".
      *    £Ó£Á£Ò£Ó¡¼£Ã£ï£Ö¡¼£²¡¦¥¤¥ó¥Õ¥ë¥¨¥ó¥¶³Ë»ÀÆ±»þ¸¡½Ð¡Ê¸¡ºº°ÑÂ÷¡Ë
           03  CONS-COV252          PIC X(09)    VALUE   "160229650".
      *    £Ó£Á£Ò£Ó¡¼£Ã£ï£Ö¡¼£²¡¦¥¤¥ó¥Õ¥ë¥¨¥ó¥¶³Ë»ÀÆ±»þ¸¡½Ð¡Ê¸¡ºº°ÑÂ÷°Ê³°¡Ë
           03  CONS-COV262          PIC X(09)    VALUE   "160229750".
      *2022/07
      *    2022.07.01¤«¤é³«»ÏÆü
      *    £Ó£Á£Ò£Ó-£Ã£ï£Ö-£²¡¦£Ò£Ó¥¦¥¤¥ë¥¹³Ë»ÀÆ±»þ¸¡½Ð¡Ê¸¡ºº°ÑÂ÷¡Ë
           03  CONS-COV272          PIC X(09)    VALUE   "160234550".
      *    £Ó£Á£Ò£Ó-£Ã£ï£Ö-£²¡¦£Ò£Ó¥¦¥¤¥ë¥¹³Ë»ÀÆ±»þ¸¡½Ð¡Ê¸¡ºº°ÑÂ÷°Ê³°¡Ë
           03  CONS-COV282          PIC X(09)    VALUE   "160234650".
      *2022/08
      *    2022.09.01¤«¤é³«»ÏÆü
      *    £Ó£Á£Ò£Ó-£Ão£Ö-£²¡¦£Ò£Ó¥¦¥¤¥ë¥¹¹³¸¶Æ±»þ¸¡½Ð¡ÊÄêÀ­¡Ë 
           03  CONS-COV134         PIC X(09)    VALUE   "160234850".
      *2022/11
      *    2022.10.28¤«¤é³«»ÏÆü
      *D023 £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²¡¦¥¤¥ó¥Õ¥ë¥¨¥ó¥¶¡¦£Ò£Ó³Ë»ÀÆ±»þ¸¡½Ð¡Ê°ÑÂ÷¡Ë
           03  CONS-COV253         PIC X(09)    VALUE   "160235250".
      *D023 £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²¡¦¥¤¥ó¥Õ¥ë¥¨¥ó¥¶¡¦£Ò£Ó³Ë»ÀÆ±»þ¸¡½Ð¡Ê°ÑÂ÷³°¡Ë
           03  CONS-COV263         PIC X(09)    VALUE   "160235350".
      *D012 £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²¡¦¥¤¥ó¥Õ¥ë¥¨¥ó¥¶¡¦£Ò£Ó¹³¸¶Æ±»þ¸¡½Ð¡ÊÄêÀ­¡Ë
           03  CONS-COV135         PIC X(09)    VALUE   "160235450".
      *
      *    ÌÈ±Ö³ØÅª¸¡ººÈ½ÃÇÎÁ
           03  CONS-COV-AUTO1      PIC X(09)    VALUE   "160062110".
      *    ÈùÀ¸Êª³ØÅª¸¡ººÈ½ÃÇÎÁ
           03  CONS-COV-AUTO2      PIC X(09)    VALUE   "160062210".
      *
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSK1014.INC".
      *    Êñ³ç¥Á¥§¥Ã¥¯¥Þ¥¹¥¿
       01  HKTSANTEI-REC.
           COPY    "CPHKTSANTEI.INC".
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥É¥Þ¥¹¥¿
       01  HKTSRYCD-REC.
           COPY    "CPHKTSRYCD.INC".
      *
      *    »»ÄêÍúÎò
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡Ý
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    Æþ±¡¹Ô°Ù¥Þ¥¹¥¿¡Ý
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    ¼õ¿ÇÍúÎò
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ÅÅ»ÒÅÀ¿ôÉ½¥Þ¥¹¥¿¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSCETNSCHK.INC".
      *
      *    Ã»´üÂÚºß¼ê½Ñ£³¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSTANKI3HKT.INC".
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYUINDAY.INC".
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ¿ÇÎÅ¹Ô°ÙÍÑ¶¦ÄÌ¥Ñ¥é¥á¥¿
           COPY    "K01COMMON-SPA".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *    Êñ³ç¿ÇÎÅ¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSCHKTCHK.INC".
      *
      *    ²èÌÌ¥¹¥ÑÎÎ°è
      *    COPY    "K02SCR-SPA".
      *    £Ó£Ð£Á¥Ñ¥é¥á¥¿
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù»»ÄêÍÑ¶¦ÄÌ¥Ñ¥é¥á¥¿(¼«Æ°È¯À¸Ê¬¥Á¥§¥Ã¥¯¡Ë
           COPY    "CPORCSC00.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSCHKTCHKAREA
           SPA-AREA
     ****  SPA-K02
           SPA-SCR-REC
           ORCSC00AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           IF      ORCSCHKTCHK-PG      =   "K02"
                                       OR  "K02N"
                                       OR  "KT01"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           ELSE
               INITIALIZE                      SPA-K01KYOUTU
           END-IF
           INITIALIZE                  ORCSCHKTCHK-OUTAREA
      *
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           EVALUATE    ORCSCHKTCHK-SYORI
               WHEN    "1"
      *        Á´ÂÎ¥Á¥§¥Ã¥¯
      *            Êñ³çÂÐ¾ÝÍ­Ìµ
                   PERFORM 100-INIT-SYORI-SEC
      *            ¥Á¥§¥Ã¥¯½èÍý
                   PERFORM 200-MAIN-SYORI-SEC
               WHEN    "2"
      *        ¼«Æ°È¯À¸Ê¬
      *            Êñ³çÂÐ¾ÝÍ­Ìµ
                   PERFORM 100-INIT-SYORI-SEC
      *            Êñ³ç¥Á¥§¥Ã¥¯
                   PERFORM 300-AUTO-SYORI-SEC
               WHEN    "3"
      *        ¥¨¥é¡¼ºï½ü
                   PERFORM 400-ERR-DELETE-SEC
               WHEN    "5"
      *        Æþ±¡ÎÁ»»Äê¥Á¥§¥Ã¥¯
      *            Êñ³çÂÐ¾ÝÍ­Ìµ
                   PERFORM 100-INIT-SYORI-SEC
      *            ¥Á¥§¥Ã¥¯½èÍý
                   PERFORM 500-NYUIN-CHKSYORI-SEC
               WHEN    "6"
      *        Æþ±¡ÎÁ»»Äê´ü´Ö¡¦Êñ³ç¥Á¥§¥Ã¥¯
      *            Êñ³çÂÐ¾ÝÍ­Ìµ
                   PERFORM 100-INIT-SYORI-SEC
      *            Æþ±¡ÎÁ¥Á¥§¥Ã¥¯½èÍý
                   IF      ORCSCHKTCHK-NYUHKTCHK   =   1
                       PERFORM 600-NYUINDAYCHK-SEC
                   ELSE
      *                Êñ³ç¶èÊ¬¥¯¥ê¥¢
                       PERFORM 602-HOUKATU-INIT-SEC
                   END-IF
                   IF      SPA-HKTSANTEI-FLG   =   1
      *                Êñ³ç¥Á¥§¥Ã¥¯È½Äê
                       PERFORM 2001-TAISYOU-CHK-SEC
                   END-IF
      *            Êñ³ç»»Äê¡ÊºÞ¡Ë¥Á¥§¥Ã¥¯
                   PERFORM 200213-HKTINPUT-CHK-SEC
      *            ºÞ¤ÎÊÔ½¸
                   PERFORM 2002-ZAIHENSYU-SEC
           END-EVALUATE
           IF      ORCSCHKTCHK-PG      =   "K02"
                                       OR  "K02N"
                                       OR  "KT01"
               MOVE    SPA-K01KYOUTU        TO  SPA-WORK
           END-IF
           . 
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SYORI-SEC            SECTION.
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷
           MOVE    SPACE               TO  SYS-1014-REC
           INITIALIZE                  SYS-1014-REC
           MOVE    "1014"              TO  SYS-1014-KANRICD
           MOVE    WRK-SRYYMD          TO  SYS-1014-STYUKYMD
           MOVE    WRK-SRYYMD          TO  SYS-1014-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
           MOVE    SYS-1014-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-N
           INITIALIZE                  TBL-AREA
           INITIALIZE                  TBL-NYUAREA
           PERFORM UNTIL  (FLG-SYSKANRI    =   1  )
                       OR (IDX             >=  100 )
               MOVE    MCPDATA-REC         TO  SYS-1014-REC
               IF      SYS-1014-SANTEI-HOU     =   "1"
                                               OR  "2"
      *            Êñ³ç»»ÄêÊýË¡¤¢¤ê
                   PERFORM 1002-KBNCD-TBLHEN1-SEC
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥ÉÊÔ½¸½èÍý
      *****************************************************************
       1002-KBNCD-TBLHEN1-SEC            SECTION.
      *
           IF      SYS-1014-KBNCD (1:3)    >=  "500"
      *        Æþ±¡ÎÁ
               ADD     1                   TO  IDX-N
               MOVE    SYS-1014-KBNCD (1:3)
                                           TO  TBL-N-KANRICD (IDX-N)
      *        Êñ³ç»»ÄêÊýË¡
               MOVE    SYS-1014-SANTEI-HOU
                                           TO  TBL-N-SANTEI-HOU (IDX-N)
               MOVE    SYS-1014-SANTEI-KBN
                                           TO  TBL-N-SANTEI-KBN (IDX-N)
               MOVE    SYS-1014-NYUGAIKBN
                                           TO  TBL-N-NYUGAIKBN (IDX-N)
               MOVE    SYS-1014-HKNCOMBI-KBN
                                           TO  TBL-N-HKNCOMBIKBN (IDX-N)
               MOVE    SYS-1014-SRYKBN-G
                                           TO  TBL-N-SRYKBN-G (IDX-N)
               PERFORM 1001-HKTSRYCD-HEN-SEC
               MOVE    IDX-N               TO  TBL-N-MAX
      *H27.9
      *        Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¡¡¤¢¤ê
               IF      SYS-1014-KBNCD (1:3)    =   "526"
                   MOVE    IDX-N               TO  WRK-TANKI3-IDX
               END-IF
           ELSE
      *        ¿ÇÎÅ¥³¡¼¥É
               ADD     1                   TO  IDX
               MOVE    SYS-1014-KBNCD (1:3)
                                           TO  TBL-KANRICD (IDX)
      *        Êñ³ç»»ÄêÊýË¡
               MOVE    SYS-1014-SANTEI-HOU
                                           TO  TBL-SANTEI-HOU (IDX)
               MOVE    SYS-1014-SANTEI-KBN
                                           TO  TBL-SANTEI-KBN (IDX)
               MOVE    SYS-1014-NYUGAIKBN
                                           TO  TBL-NYUGAIKBN (IDX)
               MOVE    SYS-1014-HKNCOMBI-KBN
                                           TO  TBL-HKNCOMBIKBN (IDX)
               MOVE    SYS-1014-SRYKBN-G
                                           TO  TBL-SRYKBN-G (IDX)
               PERFORM 1001-HKTSRYCD-HEN-SEC
               MOVE    IDX                 TO  TBL-MAX
           END-IF
           .
       1002-KBNCD-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥ÉÊÔ½¸½èÍý
      *****************************************************************
       1001-HKTSRYCD-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  HKTSRYCD-REC
           INITIALIZE                  HKTSRYCD-REC
           MOVE    SYS-1014-KBNCD (1:3)    TO  HKTSRYCD-KANRICD
           MOVE    WRK-SRYYMD          TO  HKTSRYCD-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  HKTSRYCD-YUKOEDYMD
           MOVE    HKTSRYCD-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSRYCD-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKTSRYCD
           END-IF
           MOVE    ZERO                TO  IDY
           PERFORM UNTIL  (FLG-HKTSRYCD    =   1  )
      *R06.6          OR  (IDY             >=  200 )
                      OR  (IDY             >=  400 )
               IF      SYS-1014-KBNCD (1:3)    >=  "500"
                   ADD     1                   TO  IDY
                   MOVE    HKTSRYCD-SRYCD      TO  TBL-N-SRYCD
                                                           (IDX-N IDY)
               ELSE
                   ADD     1                   TO  IDY
                   MOVE    HKTSRYCD-SRYCD      TO  TBL-SRYCD(IDX IDY)
      *
               END-IF
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSRYCD-READ-SEC
           END-PERFORM
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1001-HKTSRYCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥á¥¤¥ó½èÍý
      *****************************************************************
       200-MAIN-SYORI-SEC            SECTION.
      *
      *    Êñ³ç¶èÊ¬¥¯¥ê¥¢
           MOVE    ZERO                TO  FLG-DEKIDAKA
      *R02.10
      *    ¥³¥í¥Ê¸¡ºº¤ÎÍ­Ìµ
           MOVE    ZERO                TO  FLG-SARSCOV
           MOVE    SPACE               TO  WRK-COV-HANDAN1
           MOVE    SPACE               TO  WRK-COV-HANDAN2
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  ) 
               MOVE    ZERO            TO  SPA-COM-HOUKATU-KFLG(IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-FLG (IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-FLG2(IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-ZAI (IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-ERR (IDX-Z)
      *        ½ÐÍè¹â¡Ê²ñ·×¡Ë¤¬¤¢¤ì¤Ð¡¢¤¹¤Ù¤Æ½ÐÍè¹â
               IF      SPA-COM-INPUTCD (IDX-Z) =   "099999905"
                   MOVE    1                   TO  FLG-DEKIDAKA
               END-IF
      *        Êñ³ç»»Äê¡ÊºÞ¡ËÈ½Äê
               IF      SPA-COM-INPUTCD (IDX-Z) =   "099999908"
                   MOVE    1                   TO  ORCSCHKTCHK-HKTFLG
               END-IF
      *H28.9
      *        »ý»²ÌôÍ½Ìó¥³¡¼¥É¤ÏÊñ³çºÞ¤È¤¹¤ë
               IF      SPA-COM-INPUTCD (IDX-Z) =   CONS-JSNYCD1
                                               OR  CONS-JSNYCD2
                                               OR  CONS-JSNYCD3
                                               OR  CONS-JSNYCD4
                                               OR  CONS-JSNYCD5
                                               OR  CONS-JSNYCD6
                                               OR  CONS-JSNYCD7
                                               OR  CONS-JSNYCD8
                   MOVE    1                   TO  ORCSCHKTCHK-HKTFLG
               END-IF
           END-PERFORM
      *
           IF      FLG-DEKIDAKA        =   ZERO
      *    Êñ³ç¥Á¥§¥Ã¥¯È½Äê
               PERFORM 2001-TAISYOU-CHK-SEC
           END-IF
      *    Êñ³ç»»Äê¡ÊºÞ¡Ë¥Á¥§¥Ã¥¯
           PERFORM 200213-HKTINPUT-CHK-SEC
      *    ºÞ¤ÎÊÔ½¸
           PERFORM 2002-ZAIHENSYU-SEC
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³çºÞÊÔ½¸½èÍý
      *****************************************************************
       2002-ZAIHENSYU-SEC            SECTION.
      *
      *    Êñ³ç¥³¡¼¥É¤¬¤¢¤ëºÞ¤Ï¤¹¤Ù¤ÆÊñ³ç¤È¤¹¤ë
           MOVE    SPA-SCR-REC         TO  HZN-SCR-REC
           MOVE    ZERO                TO  IDX
           MOVE    1                   TO  IDX-S
           MOVE    ZERO                TO  IDX-ERR
           MOVE    ZERO                TO  FLG-HOUKATU
           MOVE    ZERO                TO  FLG-SONOTA
           MOVE    ZERO                TO  ORCSCHKTCHK-HKTERR
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE   )   OR
                             ((HZN-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (HZN-COM-INPUTCD (IDX-Z) =   SPACE ))
               IF     (SPA-COM-HOUKATU-ERR (IDX-Z) =   1  )
                   MOVE    1                   TO  ORCSCHKTCHK-HKTERR
               END-IF
               IF     (SPA-COM-HOUKATU-FLG (IDX-Z) =   1  )
      *            ¥³¥á¥ó¥È°Ê³°
                   IF     (SPA-COM-INPUTCD (IDX-Z)(1:1)  =   "8" )
                       OR (SPA-COM-INPUTCD (IDX-Z)(1:3)  =   "008")
                       OR (SPA-GMN-INPUTCD (IDX-Z)(1:1)  =   "."  )
      *ver.4.7         ¡Ê¥µ¡Ë¤ÏÊñ³çÂÐ¾Ý¤È¤¹¤ë
                       IF     SPA-COM-INPUTCD(IDX-Z) =   "820000046"
                           MOVE    1                   TO  FLG-HOUKATU
                       END-IF
      *????
                       CONTINUE
                   ELSE
      *H26.3
      *            Æþ±¡»»ÄêÆü»ØÄê
                       IF     (SPA-GMN-INPUTCD (IDX-Z)(1:1)
                                                   =   "*"  )
                           CONTINUE
                       ELSE
                           MOVE    1                   TO  FLG-HOUKATU
                       END-IF
                   END-IF
               END-IF
               IF     (SPA-COM-HOUKATU-FLG (IDX-Z) =   ZERO  )
                   IF    ((SPA-COM-INPUTCD (IDX-Z)(1:1)    =   "1" )
                      AND (SPA-COM-DATAKBN (IDX-Z)         =   "1" ))
                   OR    ( SPA-COM-INPUTCD (IDX-Z)(1:1)    =   "6"
                                                           OR  "7" )
                   OR    ( SPA-COM-HOUKATU-FLG2 (IDX-Z)    =   1   )
                       MOVE    1                   TO  FLG-SONOTA
                   END-IF
               END-IF
               IF     (SPA-COM-ZAIEND (IDX-Z)  =   "1"   )
                   PERFORM VARYING     IDX     FROM    IDX-S  BY  1
                           UNTIL      (IDX         >   IDX-Z  )
                       MOVE    FLG-HOUKATU     TO  SPA-COM-HOUKATU-ZAI
                                                              (IDX)
                       IF      FLG-HOUKATU     =   ZERO
                           MOVE    ZERO        TO  SPA-COM-HOUKATU-FLG
                                                              (IDX)
                       END-IF
      *                ºÞÆâ¤ËÊñ³ç³°¤¬¤¢¤ë¾ì¹ç¡¢·Ù¹ð
                       IF     (FLG-HOUKATU     =   1   )  AND
                              (FLG-SONOTA      =   1   )  AND
                              (SPA-COM-HOUKATU-FLG (IDX) =   ZERO)
                           PERFORM 2002-HOUKATU-GAI-CHK-SEC
                       ELSE
                           MOVE    ZERO        TO  SPA-COM-HOUKATU-FLG2
                                                                (IDX) 
                       END-IF
                   END-PERFORM
                   MOVE    IDX                 TO  IDX-S
                   MOVE    ZERO                TO  FLG-HOUKATU
                   MOVE    ZERO                TO  FLG-SONOTA
               END-IF
           END-PERFORM
      *
           IF      IDX-ERR         NOT =   ZERO
               IF      SPA-HKTKEI-FLG      =   ZERO
                   MOVE    "K202"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-HKTKEI-FLG
                   MOVE    "1"             TO  SPA-COM-CUR(IDX-ERR)
               END-IF
           END-IF
      *
           .
       2002-ZAIHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç³°¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2002-HOUKATU-GAI-CHK-SEC            SECTION.
      *
           EVALUATE    WRK-KANRICD
           WHEN    "001"
      *        ³°Íè¿ÇÎÅÎÁ¤ÏÌôºÞ¡¦´ïºà¤Ï»»Äê²Ä
               IF    (SPA-COM-INPUTCD (IDX)(1:1)   =   "6"
                                                   OR  "7" )
                   MOVE    1           TO  SPA-COM-HOUKATU-FLG2 (IDX)
               END-IF
           WHEN    "201"
      *        ËýÀ­°Ý»ýÆ©ÀÏ´µ¼ÔÎÁ¤ÏÌôºÞ¡¦´ïºà¤Ï»»Äê²Ä
               IF    (SPA-COM-INPUTCD (IDX)(1:1)   =   "6"
                                                   OR  "7" )
                   MOVE    1           TO  SPA-COM-HOUKATU-FLG2 (IDX)
               END-IF
           WHEN    "208"
           WHEN    "525"
      *        Ã»´üÂÚºß¼ê½Ñ£±¡¢£²¤Î¥Õ¥£¥ë¥à¤Ï»»Äê²Ä
               IF    (SPA-COM-SRYKBN  (IDX)        =   "70")  AND
                     (SPA-COM-INPUTCD (IDX)(1:1)   =   "7" )  AND
                     (SPA-COM-DATAKBN (IDX)        =   "3" )
                   MOVE    1           TO  SPA-COM-HOUKATU-FLG2 (IDX)
               END-IF
           END-EVALUATE
      *
           IF     (SPA-COM-INPUTCD (IDX)(1:1)  =   "1" ) AND
                  (SPA-COM-DATAKBN (IDX)       =   "1" )
               IF     (SPA-COM-SRYKBN (IDX)    =   "60"  ) AND
                      (SPA-COM-KENTAICOMMENT(IDX)  =   1 )
      *            ¸¡ÂÎ¸¡ºº¥³¥á¥ó¥È¤ÏÂÐ¾Ý³°
                   CONTINUE
               ELSE
                   MOVE    1               TO  SPA-COM-HOUKATU-FLG2
                                                                (IDX)
               END-IF
           END-IF
      *    ËýÀ­°Ý»ýÆ©ÀÏ´µ¼Ô´ÉÍýÎÁ¤Î²èÁü¿ÇÃÇ
           IF     (WRK-KANRICD             =   "201"  )
           AND    (SPA-COM-SRYKBN (IDX)    =   "70" )
           AND    (SPA-COM-INPUTCD(IDX)(1:1)   =   "1" )
      *        ¿ÇÃÇÎÁÁ°¤Î»£±ÆÎÁ¤ÏÊñ³ç¤Ø
               IF     (SPA-COM-DATAKBN (IDX)       =   "1" )
               AND    (SPA-COM-TNSSRYSYUKBN(IDX)   =   "721" )
                   MOVE    ZERO            TO  SPA-COM-HOUKATU-FLG2
                                                                (IDX)
                   MOVE    1               TO  SPA-COM-HOUKATU-FLG
                                                                (IDX)
               END-IF
      *        ¼«Æ°È¯À¸°Ê³°¤Î²Ã»»¤Ï»»Äê²Ä
               IF     (SPA-COM-DATAKBN (IDX)   =   "2"   )
                 AND  (SPA-COM-AUTOKBN (IDX)   =   SPACE )
                   MOVE    1               TO  SPA-COM-HOUKATU-FLG2
                                                                (IDX)
               END-IF
           END-IF
      *
           IF      SPA-COM-HOUKATU-FLG2 (IDX)  =   1
               IF      IDX-ERR             =   ZERO
                   MOVE    IDX                 TO  IDX-ERR
               END-IF
           END-IF
           .
       2002-HOUKATU-GAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³çÈ½Äê¡¦¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-TAISYOU-CHK-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   TBL-MAX )
               MOVE    ZERO            TO  FLG-CHK
      *!!!!
      *        Æþ³°¶èÊ¬È½Äê¡ÊÆþ±¡ÎÁ°Ê³°¤ÎÆþ³°¶èÊ¬ÊÌ¤Ï³°Íè¤Î¤ß¥Á¥§¥Ã¥¯¡Ë
      *               Æþ±¡»»ÄêÉÔ²Ä¤Î¥³¡¼¥É¤Î¤ß¤¬ÂÐ¾Ý¤Î°Ù¡£
      *               Î¾Êý¤Ç¤Î»»Äê²Ä¤¬¤¢¤ì¤ÐÂÐ±þ¤¬É¬Í×
               IF     (TBL-NYUGAIKBN (IDX)     =   "1" )
                 AND  (SPA-NYUGAIKBN           =   1   )
                   MOVE    2               TO  FLG-CHK
               END-IF
      *!!!!
               PERFORM VARYING     IDX-Z   FROM    1   BY  1
                       UNTIL      (IDX-Z       >   SPA-GMN-MAX )
                               OR (FLG-CHK NOT =   ZERO )
                   IF     (SPA-COM-HKNCOMBI (IDX-Z)        =   "9999")
                      OR  (SPA-COM-SRYKBN (IDX-Z)(1:1)     =   "9"   )
      *               ¼£¸³ÂÐ¾Ý³°
                      OR  (SPA-COM-CHIKENKBN(IDX-Z)        =   "1"   )
      *               ÌôÉ¾ÂÐ¾Ý³°
                      OR  (SPA-COM-YAKUHYO  (IDX-Z)        =   "1"   )
                      OR  (SPA-COM-GENTEN   (IDX-Z)        =   "1"   )
      *               ¥¢¥Õ¥¿¡¼¥±¥¢
                      OR  (SPA-RSI-HKNKBN                  =   "3" )
                       CONTINUE
                    ELSE
      *                ¼êµ»
                       IF     (SPA-COM-INPUTCD(IDX-Z)(1:1) =   "1" )
                           MOVE    SPA-COM-INPUTCD(IDX-Z)  TO  WRK-SRYCD
                           PERFORM 2001-HKTSRYCD-CHK-SEC
                           IF      FLG-CHK2            =   1
                               MOVE    1               TO  FLG-CHK
      *H29.9
      *                        ·îËè¤ÎÊñ³ç¥Á¥§¥Ã¥¯
                               IF      TBL-SANTEI-KBN(IDX) =   "3"
                                   PERFORM 2002-SANTEI03-CHEK-SEC
                               END-IF
      *
                           END-IF
                       ELSE
      *                ³°Íè¿ÇÎÅÎÁ¥Á¥§¥Ã¥¯¤ÏºÆ¿Ç¶èÊ¬¤¬¤¢¤ì¤ÐÂÐ¾Ý
                           IF     (TBL-KANRICD (IDX)       =   "001")
                              AND (SPA-COM-SRYKBN (IDX-Z)  =   "12" )
      *                       °ìÈÌÉÂ¾²¿ô
                              AND  (SPA-BEDSUIPN        >=  200 )
                              AND  (SPA-HOSPSBT         =   1   )
                               MOVE    1               TO  FLG-CHK
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               IF     (FLG-CHK                 =   ZERO )
                  AND (TBL-SANTEI-KBN(IDX) NOT =   "1" )
      *            »»ÄêÍúÎò¤Ë¤è¤ëÊñ³ç¿ÇÎÅ¥Á¥§¥Ã¥¯
                   PERFORM 2003-HKTSRYCD-RRK-SEC
               END-IF
               IF      FLG-CHK             =   1
                   MOVE    1                   TO  ORCSCHKTCHK-HKTFLG
                   MOVE    TBL-KANRICD (IDX)       TO  WRK-KANRICD
                   MOVE    TBL-SANTEI-HOU (IDX)    TO  WRK-SANTEI-HOU
                   MOVE    TBL-SANTEI-KBN (IDX)    TO  WRK-SANTEI-KBN
                   MOVE    TBL-NYUGAIKBN (IDX)     TO  WRK-NYUGAIKBN
                   MOVE    TBL-HKNCOMBIKBN (IDX)   TO  WRK-HKNCOMBIKBN
                   MOVE    TBL-SRYKBN-G    (IDX)   TO  WRK-SRYKBN-G
      *            ÆþÎÏ¹àÌÜ¤ÎÊñ³ç¥Á¥§¥¯
                   PERFORM 2002-HKTSANTEI-CHK-SEC
               END-IF
           END-PERFORM
      *
      *!!!!!!!!!
      *H27.9
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¤Î³°Íè¥Á¥§¥Ã¥¯
           IF     (WRK-TANKI3-IDX  NOT =   ZERO )
      *H30.10
      *****   AND (SPA-NYUGAIKBN       =   2    )
               MOVE    1                   TO  ORCSCHKTCHK-HKTFLG
               PERFORM 20013-TANKI3HKT-SEC
           END-IF
           .
       2001-TAISYOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç¥³¡¼¥É¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-HKTSRYCD-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK2
           PERFORM VARYING     IDY     FROM    1   BY  1
      ***          UNTIL      (IDY     >   200   )  OR
                   UNTIL      (IDY     >   400   )  OR
                              (TBL-SRYCD (IDX IDY) =   SPACE)
               IF      WRK-SRYCD           =   TBL-SRYCD (IDX IDY)
                   MOVE    TBL-SRYCD (IDX IDY) TO  WRK-K-SRYCD
                   MOVE    1                   TO  FLG-CHK2
      **           MOVE    200                 TO  IDY
                   MOVE    400                 TO  IDY
               END-IF
           END-PERFORM
      *
           .
       2001-HKTSRYCD-CHK-EXT.
           EXIT.
      *
      *H29.9
      *****************************************************************
      *    ·îËè¤ÎÊñ³ç¥Á¥§¥Ã¥¯¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2002-SANTEI03-CHEK-SEC            SECTION.
      *
           IF     (ORCSCHKTCHK-PG      =   "K02" )
               IF      SPA-SYORIFLG       =   0
      *            ¿·µ¬
                   PERFORM 20021-SANTEI03-CHEK02-SEC
               ELSE
      *            ÄûÀµ
                   PERFORM 20022-SANTEI03-CHEK03-SEC
               END-IF
           END-IF
           .
       2002-SANTEI03-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ·îËè¤ÎÊñ³ç¥Á¥§¥Ã¥¯¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       20021-SANTEI03-CHEK02-SEC            SECTION.
      *
      *    Åö·î¤Î¼õ¿ÇÍúÎòÅÐÏ¿È½Äê
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      *    £±Æü¤«¤é·îËö¤Þ¤Ç
           MOVE    WRK-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    WRK-SRYYMD          TO  JYURRK-UPSRYYMD
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key33"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  FLG-JYURRK-CHK
           IF      SPA-SYORIFLG        =   1
      *        ÄûÀµ
               PERFORM UNTIL      (FLG-JYURRK      =   1 )
                            OR    (FLG-JYURRK-CHK  =   1 )
                   IF     (SPA-DENPNUM         =   JYURRK-DENPNUM)
                      OR  (SPA-DENPNUM         =   JYURRK-GRP-DENPNUM)
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-JYURRK-CHK
                   END-IF
                   MOVE    "key33"             TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-READ-SEC
               END-PERFORM
           ELSE
               IF      FLG-JYURRK          =   ZERO
                   MOVE    1               TO  FLG-JYURRK-CHK
               END-IF
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-JYURRK-CHK      =   1
               IF      SPA-COM-KZMFLG9  (IDX-Z)  =   SPACE
                   MOVE   "K300"           TO  SPA-ERRCD
                   MOVE   "1"              TO  SPA-COM-CUR
                                                      (IDX-Z)
                   MOVE   "1"              TO  SPA-COM-KZMFLG9
                                                      (IDX-Z)
               END-IF
           END-IF
      *
           .
       20021-SANTEI03-CHEK02-EXT.
           EXIT.
      *****************************************************************
      *    ÄûÀµ»þ¡¢Êñ³ç¿ÇÎÅ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       20022-SANTEI03-CHEK03-SEC            SECTION.
      *
           MOVE    ZERO                    TO  FLG-TEISEI-ARI
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   (IDZ        >   SPA-MAX-LINE )  OR
                           (LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   SPACE )
                        OR (FLG-TEISEI-ARI =   1     )
               IF      LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   WRK-SRYCD
                   MOVE    1               TO  FLG-TEISEI-ARI
               END-IF
               PERFORM VARYING     IDY     FROM    1   BY  1
      *R06.4           UNTIL      (IDY     >   200   )  OR
                       UNTIL      (IDY     >   400   )  OR
                                  (TBL-SRYCD (IDX IDY) =   SPACE)
                                OR (FLG-TEISEI-ARI =   1     )
                   IF       LNK-ORCSC-TEI-SRYCD(IDZ)
                                               =   TBL-SRYCD (IDX IDY)
                       MOVE    1               TO  FLG-TEISEI-ARI
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    Á°²ó¤ÈÆ±¤¸Êñ³ç¤Ï¥Á¥§¥Ã¥¯¤Ê¤·
           IF      FLG-TEISEI-ARI      =   ZERO
               PERFORM 20021-SANTEI03-CHEK02-SEC
           END-IF
      *
           .
       20022-SANTEI03-CHEK03-EXT.
           EXIT.
      *H29.9
      *
      *
      *****************************************************************
      *    »»ÄêÍúÎò¤Ë¤è¤ëÊñ³ç¿ÇÎÅ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2003-HKTSRYCD-RRK-SEC            SECTION.
      *
      *    ³°Íè¤Þ¤È¤á¤Ï¥Ñ¥é¥á¥¿¤ÇÈ½Äê
           IF      ORCSCHKTCHK-KT01FLG =   "1"
               PERFORM 20030-KT01-HKTSRYCD-CHK-SEC
               GO      TO      2003-HKTSRYCD-RRK-EXT
           END-IF
      *
      *    »»ÄêÍúÎò¸¡º÷
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYYM           TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM
               UNTIL      (FLG-SANTEI      =   1  )  OR
                          (FLG-CHK         =   1  )
               MOVE    SANTEI-SRYCD            TO  WRK-SRYCD
               PERFORM 2001-HKTSRYCD-CHK-SEC
               IF     (SANTEI-MSANTEICNT   >   ZERO )
                 AND  (FLG-CHK2            =   1    )
      *            ÊÝ¸±ÊÌ¤Î»þ¡¢ÊÝ¸±ÁÈ¹ç¤»È½Äê
                   IF      (TBL-HKNCOMBIKBN(IDX)   =   "1" )
                       AND (SANTEI-HKNCOMBINUM NOT =   SPA-HKNCOMBINUM)
                       CONTINUE
                   ELSE
                       PERFORM 20031-SANTEI-CHK-SEC
                   END-IF
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2003-HKTSRYCD-RRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Íè¤Þ¤È¤á¤Î»»ÄêÍúÎò¤Ë¤è¤ëÊñ³ç¿ÇÎÅ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       20030-KT01-HKTSRYCD-CHK-SEC            SECTION.
      *
           PERFORM VARYING     IDX-SN      FROM    1   BY  1
                   UNTIL      (IDX-SN      >   100 )
                        OR    (ORCSCHKTCHK-KT01-SRYCD (IDX-SN)
                                           =   SPACE)
                        OR    (FLG-CHK         =   1  )
               INITIALIZE                  SANTEI-REC
               MOVE    SPA-SRYYMD(1:6)     TO  SANTEI-SRYYM
               PERFORM VARYING     IDD         FROM    1   BY  1
                       UNTIL      (IDD         >   31  )
                   MOVE    ORCSCHKTCHK-KT01-DAY (IDX-SN IDD)
                                               TO  SANTEI-DAY (IDD)
                   ADD     SANTEI-DAY (IDD)    TO  SANTEI-MSANTEICNT
                   IF      SANTEI-DAY (IDD)    >   ZERO
                       IF      SANTEI-MSANTEID     =   SPACE
                           MOVE    IDD            TO  SANTEI-MSANTEID
                       END-IF
                   END-IF
               END-PERFORM
              MOVE    ORCSCHKTCHK-KT01-SRYCD  (IDX-SN)
                                               TO  WRK-SRYCD
               PERFORM 2001-HKTSRYCD-CHK-SEC
               IF     (SANTEI-MSANTEICNT   >   ZERO )
                 AND  (FLG-CHK2            =   1    )
      *            ÊÝ¸±ÊÌ¤Î»þ¡¢ÊÝ¸±ÁÈ¹ç¤»È½Äê
      *            IF      (TBL-HKNCOMBIKBN(IDX)   =   "1" )
      *                AND (SANTEI-HKNCOMBINUM NOT =   SPA-HKNCOMBINUM)
      *                CONTINUE
      *            ELSE
                       PERFORM 20031-SANTEI-CHK-SEC
      *            END-IF
               END-IF
           END-PERFORM
           .
       20030-KT01-HKTSRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÍúÎò¤Î»»Äê¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       20031-SANTEI-CHK-SEC            SECTION.
      *
      *    ÄûÀµ¤Î»þ¡¢ÅöÆü»»ÄêÍ­Ìµ¤ÎÈ½Äê
           MOVE    ZERO                TO  WRK-TEI-CNT
           IF      SPA-SYORIFLG        =   1
               PERFORM VARYING IDZ     FROM    1   BY  1
                       UNTIL   (IDZ        >   SPA-MAX-LINE )  OR
                               (LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   SPACE )
                   IF      LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   WRK-SRYCD
                       ADD     LNK-ORCSC-TEI-KAISU(IDZ)
                                               TO  WRK-TEI-CNT
                   END-IF
               END-PERFORM
           END-IF
           MOVE    WRK-SRYDD           TO  IDD2
           EVALUATE    TBL-SANTEI-KBN(IDX)
               WHEN    "2"
      *            »»ÄêÆü
                   IF     (SANTEI-DAY (IDD2)   =   ZERO )  OR
                          (SANTEI-DAY (IDD2)   =   WRK-TEI-CNT)
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-CHK
      *H27.8
      *            ¾®»ù²Ê³°¿ÇÎÅÎÁ»»ÄêÆü¤ÎÏ«ºÒ¡¦¼«ÇåÀÕ¤ÏÂÐ¾Ý³°
                       IF     (TBL-KANRICD (IDX)   =   "202" )
                          AND (SPA-SYONIKA-ARI     =   ZERO  )
                          AND (SPA-HKNKBN          =   1     )
                           MOVE    ZERO            TO  FLG-CHK
                       END-IF
      *H30.8
      *            ¾®»ù²Ê¤«¤«¤ê¤Ä¤±¿ÇÎÅÎÁ»»ÄêÆü¤ÎÏ«ºÒ¡¦¼«ÇåÀÕ¤ÏÂÐ¾Ý³°
                       IF     (TBL-KANRICD (IDX)   =   "209" )
                          AND (SPA-SYONIKKR-ARI    =   ZERO  )
                          AND (SPA-HKNKBN          =   1     )
                           MOVE    ZERO            TO  FLG-CHK
                       END-IF
                   END-IF
               WHEN    "3"
      *            »»Äê·î
                   COMPUTE WRK-DAY             =   SANTEI-MSANTEICNT
                                               -   WRK-TEI-CNT
                   IF      WRK-DAY             >   ZERO
                       MOVE    1               TO  FLG-CHK
                   END-IF
               WHEN    "4"
      *            »»ÄêÆü°Ê¹ß
                   PERFORM VARYING     IDD     FROM    1   BY  1
                           UNTIL       IDD     >   IDD2
                       IF     (SANTEI-DAY (IDD)    >   ZERO )
                           IF      (IDD            =   IDD2 )  AND
                                   (SANTEI-DAY (IDD)    =   WRK-TEI-CNT)
                               CONTINUE
                           ELSE
                               MOVE    1               TO  FLG-CHK
                           END-IF
                           MOVE    IDD2                TO  IDD
                       END-IF
                   END-PERFORM
           END-EVALUATE
           .
       20031-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆþÎÏ¹àÌÜ¤ÎÊñ³ç¥Á¥§¥¯½èÍý
      *****************************************************************
       2002-HKTSANTEI-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  HKTSANTEI-REC
           INITIALIZE                  HKTSANTEI-REC
           MOVE    WRK-KANRICD         TO  HKTSANTEI-KANRICD
           MOVE    WRK-SRYYMD          TO  HKTSANTEI-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  HKTSANTEI-YUKOEDYMD
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    HKTSANTEI-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_hktsantei"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hktsantei"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSANTEI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKTSANTEI
           END-IF
           INITIALIZE                  TBL-HKTSANTEI-AREA
           MOVE    ZERO                TO  IDX-H
      *    ÅÅ»ÒÅÀ¿ôÉ½ÂÐ¾ÝÊ¬¤«¤éÂÐ¾Ý³°
           IF      WRK-SRYYMD          >=  "20100401"
               MOVE    1               TO  FLG-HKTSANTEI
           END-IF
      *
           PERFORM UNTIL  (FLG-HKTSANTEI   =   1  )
                      OR  (IDX-H           >=  600 )
               ADD     1                   TO  IDX-H
               MOVE    HKTSANTEI-SRYCD     TO  TBL-HKTSRYCD (IDX-H)
               MOVE    HKTSANTEI-SANTEIKBN TO  TBL-SANTEIKBN (IDX-H)
               MOVE    HKTSANTEI-KENTAIKBN TO  TBL-KENTAIKBN (IDX-H)
               MOVE    "tbl_hktsantei"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_hktsantei"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  WRK-HKT-SRYKBN-G
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL      (IDX-K       >   13  )
               IF      WRK-SRYKBN-KBN (IDX-K)  =   "1"
                   EVALUATE    IDX-K
                       WHEN    1
                           MOVE    1           TO  WRK-HKTKBN20
                       WHEN    2
                           MOVE    1           TO  WRK-HKTKBN30
                       WHEN    3
                           MOVE    1           TO  WRK-HKTKBN40
                       WHEN    4
                           MOVE    1           TO  WRK-HKTKBN50
                       WHEN    5
                           MOVE    1           TO  WRK-HKTKBN60
                       WHEN    6
                           MOVE    1           TO  WRK-HKTKBN70
                       WHEN    7
                           MOVE    1           TO  WRK-HKTKBN80
                       WHEN    8
                           MOVE    1           TO  WRK-HKTKBN81
                       WHEN    9
                           MOVE    1           TO  WRK-HKTKBN82
                       WHEN    10
                           MOVE    1           TO  WRK-HKTKBN64
                       WHEN    11
                           MOVE    1           TO  WRK-HKTKBN13
                       WHEN    12
                           MOVE    1           TO  WRK-HKTKBN14
                       WHEN    13
                           MOVE    1           TO  WRK-HKTKBN10
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           IF      ORCSCHKTCHK-SYORI   =   "1"
                                       OR  "6"
      *        ÆþÎÏ¹àÌÜ
               PERFORM 20021-INPUT-CHK-SEC
           ELSE
      *        ¼«Æ°È¯À¸
               PERFORM 30021-AUTO-CHK-SEC
           END-IF
           .
       2002-HKTSANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆþÎÏ¹àÌÜÊñ³ç¿ÇÎÅ¥³¡¼¥É¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       20021-INPUT-CHK-SEC            SECTION.
      *
      *    ¿ÇÎÅ¶èÊ¬¤Ë¤è¤ëÊñ³ç
           MOVE    ZERO                TO  FLG-SANTEIOK
           MOVE    ZERO                TO  FLG-INPUTOK
           MOVE    ZERO                TO  FLG-INPUTOK2
           MOVE    ZERO                TO  FLG-INPUTOK6
           MOVE    ZERO                TO  FLG-INPUTOK7
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
               IF     (SPA-COM-HKNCOMBI (IDX-Z)    =   "9999")
                   OR (SPA-COM-HOUKATU-FLG(IDX-Z)  =   1     )
                   OR (SPA-COM-HOUKATU-ERR(IDX-Z)  =   1     )
                   OR (SPA-GMN-INPUTCD (IDX-Z)(1:1)
                                                   =   "#"
                                                   OR  "." 
                                                   OR  "$"   )
      *            ¼£¸³ÂÐ¾Ý³°
                   OR (SPA-COM-CHIKENKBN(IDX-Z)    =   "1"   )
      *            ÌôÉ¾ÂÐ¾Ý³°
                   OR  (SPA-COM-YAKUHYO (IDX-Z)    =   "1"   )
                   OR  (SPA-COM-GENTEN  (IDX-Z)    =   "1"   )
      *            ¥¢¥Õ¥¿¡¼¥±¥¢
                   OR  (SPA-RSI-HKNKBN             =   "3" )
                   CONTINUE
               ELSE
                   IF     (SPA-COM-INPUTCD (IDX-Z)(1:1) =   "1"
                                                        OR  "6"
                                                        OR  "7" )
                       MOVE    1                   TO  FLG-INPUTOK
      *H26.11
      *                ÅÅ»ÒÅÀ¿ôÉ½¤«¤é¤Î¥Á¥§¥Ã¥¯¤Ï¡¢ÌôºÞ¡¦´ïºà¤Î¤ß
                       IF     (TBL-HKTSRYCD (1)        =   SPACE )
                           IF      SPA-COM-INPUTCD (IDX-Z)(1:1) =   "6"
                                                                OR  "7"
                               MOVE    1               TO  FLG-INPUTOK6
                           END-IF
                       END-IF
                   END-IF
      *H24.8       ¡Ê¥µ¡Ë¡¢¥æ¡¼¥¶´ïºà
                   IF     (SPA-COM-INPUTCD (IDX-Z)     =   "820000046")
      ***************     (SPA-COM-INPUTCD (IDX-Z)(1:3)    = "059"  )
                       MOVE    1                   TO  FLG-INPUTOK
                       MOVE    1                   TO  FLG-INPUTOK2
                   END-IF
      *H30.1       ¥æ¡¼¥¶´ïºà
                   IF     (SPA-COM-INPUTCD (IDX-Z)(1:3)    = "059"  )
                       MOVE    1                   TO  FLG-INPUTOK
                       MOVE    1                   TO  FLG-INPUTOK7
                   END-IF
      *H24.8
      *            ºÞÅÀ¿ô¤¬¤¢¤ëºÞ¤Î¤ß
                   IF     (SPA-COM-ZAIEND (IDX-Z)  =   "1"  )
      *************  AND  (SPA-COM-TENSU  (IDX-Z)  >   ZERO )
                     AND  (FLG-SANTEIOK            =   ZERO )
                     AND  (FLG-INPUTOK             =   1    )
      *H26.11
      *                ÅÅ»ÒÅÀ¿ôÉ½¤«¤é¤Î¥Á¥§¥Ã¥¯¤Ï¡¢ÌôºÞ¡¦´ïºà¤Î¤ß
                       IF     (TBL-HKTSRYCD (1)        =   SPACE )
                         AND  (FLG-INPUTOK6            =   ZERO  )
                         AND  (FLG-INPUTOK7            =   ZERO  )
                           CONTINUE
                       ELSE
                           PERFORM 20022-SRYKBN-CHECK-SEC
                       END-IF
                   END-IF
               END-IF
      *        ½ÐÍè¹â»»ÄêÈ½Äê
               IF      SPA-COM-ZAIEND (IDX-Z)  =   "1"
                   MOVE    ZERO                TO  FLG-SANTEIOK 
                   MOVE    ZERO                TO  FLG-INPUTOK
                   MOVE    ZERO                TO  FLG-INPUTOK2
      *H30.1
                   MOVE    ZERO                TO  FLG-INPUTOK6
                   MOVE    ZERO                TO  FLG-INPUTOK7
               END-IF
               IF      SPA-COM-INPUTCD (IDX-Z) =   "099999903"
                   MOVE    1                   TO  FLG-SANTEIOK
               END-IF
           END-PERFORM
      *
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥É¥Á¥§¥Ã¥¯
           MOVE    ZERO                TO  FLG-SANTEIOK
      *R02.10
           MOVE    ZERO                TO  FLG-SANTEINO
      *    ¥³¥í¥Ê¸¡ºº¤ÎÍ­Ìµ
           MOVE    ZERO                TO  FLG-SARSCOV
           MOVE    SPACE               TO  WRK-COV-HANDAN1
           MOVE    SPACE               TO  WRK-COV-HANDAN2
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
               IF     (SPA-COM-HKNCOMBI (IDX-Z)    =   "9999")
                   OR (SPA-COM-HOUKATU-FLG(IDX-Z)  =   1     )
                   OR (SPA-COM-HOUKATU-ERR(IDX-Z)  =   1     )
                   OR (SPA-GMN-INPUTCD (IDX-Z)(1:1)
                                                   =   "#"
                                                   OR  "$"   )
      *            ¼£¸³ÂÐ¾Ý³°
                   OR (SPA-COM-CHIKENKBN(IDX-Z)    =   "1"   )
      *            ÌôÉ¾ÂÐ¾Ý³°
                   OR  (SPA-COM-YAKUHYO (IDX-Z)    =   "1"   )
                   OR  (SPA-COM-GENTEN  (IDX-Z)    =   "1"   )
      *            ¥¢¥Õ¥¿¡¼¥±¥¢
                   OR  (SPA-RSI-HKNKBN             =   "3" )
                   CONTINUE
               ELSE
      *
      *R02.10
      *            £Ð£Ã£Ò¸¡ºº¤ÎÈ½ÃÇÎÁ¤Ï½ÐÍè¹â¤È¤¹¤ë¡Ê¸¡ºº¤Î¸å¤ËÆþÎÏ¡Ë
                   IF     (SPA-COM-INPUTCD(IDX-Z)(1:1) =   "1"  )
                     AND  (SPA-COM-INPUTCD(IDX-Z) =   WRK-COV-HANDAN1
                                                  OR  WRK-COV-HANDAN2)
                       MOVE    1               TO  FLG-SANTEIOK
                   END-IF
      *
                   IF     (SPA-COM-INPUTCD (IDX-Z)(1:1) =   "1" )
                   AND    (FLG-SANTEIOK        =   ZERO )
      *
                       MOVE    SPA-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
                       IF     (TBL-HKTSRYCD (1)        =   SPACE )
      *                    ÅÅ»ÒÅÀ¿ôÉ½¤«¤é¤Î¥Á¥§¥Ã¥¯
                           PERFORM 20028-ETENSU-CHECK-SEC
                       ELSE
      *                    Êñ³ç¥Á¥§¥Ã¥¯¥Þ¥¹¥¿¤«¤é¤Î¥Á¥§¥Ã¥¯
                           PERFORM 20021-SRYCD-CHECK-SEC
                       END-IF
      *
                   END-IF
               END-IF
      *R02.10
      *        ½ÐÍè¹â¤Î£Ð£Ã£Ò¸¡ººÈ½Äê
               IF     (SPA-COM-HOUKATU-FLG(IDX-Z)  =   ZERO )
                 AND  (SPA-COM-HOUKATU-FLG2(IDX-Z) =   ZERO )
                 AND  (SPA-COM-HOUKATU-ERR (IDX-Z) =   ZERO )
                 AND  (SPA-COM-INPUTCD(IDX-Z)(1:1) =   "1"  )
                 AND  (FLG-SANTEINO        =    ZERO      )
                 AND ((SPA-SRYYMD          >=  "20200901")  AND
                      (SPA-SRYYMD          <   "20240601"))
                   PERFORM 200214-PCRKENSA-CHK-SEC
               END-IF
      *
      *        ½ÐÍè¹â»»ÄêÈ½Äê
               IF      SPA-COM-ZAIEND (IDX-Z)  =   "1"
                   MOVE    ZERO                TO  FLG-SANTEIOK
                   MOVE    ZERO                TO  FLG-SANTEINO
               END-IF
               IF      SPA-COM-INPUTCD (IDX-Z) =   "099999903"
                   MOVE    1                   TO  FLG-SANTEIOK
               END-IF
      *R02.10  Êñ³ç»»ÄêºÞ
               IF      SPA-COM-INPUTCD (IDX-Z) =   "099999908"
                   MOVE    1                   TO  FLG-SANTEINO
               END-IF
           END-PERFORM
      *
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥ÉÊÔ½¸
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
               IF     (SPA-COM-HOUKATU-KFLG (IDX-Z)  =   1     )
                   OR (SPA-COM-HOUKATU-KFLG2(IDX-Z)  =   1     )
                   OR (SPA-COM-HOUKATU-KERR (IDX-Z)  =   1     )
                   MOVE    SPA-COM-HOUKATU-KFLG(IDX-Z)
                                       TO  SPA-COM-HOUKATU-FLG (IDX-Z)
                   MOVE    SPA-COM-HOUKATU-KFLG2(IDX-Z)
                                       TO  SPA-COM-HOUKATU-FLG2 (IDX-Z)
                   MOVE    SPA-COM-HOUKATU-KERR(IDX-Z)
                                       TO  SPA-COM-HOUKATU-ERR (IDX-Z)
               END-IF
               MOVE    ZERO                TO  SPA-COM-HOUKATU-KFLG
                                                               (IDX-Z)
               MOVE    ZERO                TO  SPA-COM-HOUKATU-KFLG2
                                                               (IDX-Z)
               MOVE    ZERO                TO  SPA-COM-HOUKATU-KERR
                                                               (IDX-Z)
           END-PERFORM
           .
       20021-INPUT-CHK-EXT.
           EXIT.
      *
      *R02.10
      *****************************************************************
      *    ½ÐÍè¹â¤Î£Ð£Ã£Ò¸¡ººÈ½Äê½èÍý
      *****************************************************************
       200214-PCRKENSA-CHK-SEC            SECTION.
      *
      *    £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²¹³¸¶¸¡½Ð
           IF      SPA-COM-INPUTCD (IDX-Z) =   CONS-COV11
                                           OR  CONS-COV12
      *R03.5
                                           OR  CONS-COV13
      *R03.12
                                           OR  CONS-COV112
                                           OR  CONS-COV122
                                           OR  CONS-COV132
      *R04.9
                                           OR  CONS-COV134
      *R04.11
                                           OR  CONS-COV135
               MOVE    1                   TO  FLG-SARSCOV
      *        ÌÈ±Ö³ØÅª¸¡ººÈ½ÃÇÎÁ
               MOVE    CONS-COV-AUTO1      TO  WRK-COV-HANDAN1
           END-IF
      *    £Ó£Á£Ò£Ó¡Ý£Ã£ï£Ö¡Ý£²³Ë»À¸¡½Ð¤Ê¤É
           IF      SPA-COM-INPUTCD (IDX-Z) =   CONS-COV21
                                           OR  CONS-COV22
                                           OR  CONS-COV23
                                           OR  CONS-COV24
      *R02.12
                                           OR  CONS-COV25
                                           OR  CONS-COV26
      *R03.12
                                           OR  CONS-COV212
                                           OR  CONS-COV222
                                           OR  CONS-COV252
                                           OR  CONS-COV262
      *R04.7
                                           OR  CONS-COV272
                                           OR  CONS-COV282
      *R04.11
                                           OR  CONS-COV253
                                           OR  CONS-COV263
               MOVE    1                   TO  FLG-SARSCOV
      *        ÈùÀ¸Êª³ØÅª¸¡ººÈ½ÃÇÎÁ
               MOVE    CONS-COV-AUTO2      TO  WRK-COV-HANDAN2
           END-IF
      *
           .
       200214-PCRKENSA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç»»Äê¡ÊºÞ¡ËÆþÎÏ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       200213-HKTINPUT-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-HKTINP
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
               IF     (SPA-COM-HKNCOMBI (IDX-Z)    =   "9999")
                   OR (SPA-COM-SRYKBN   (IDX-Z)    =   "95"
                                                   OR  "96"
                                                   OR  "98"
                                                   OR  "99"  )
                   CONTINUE
               ELSE
      *            Êñ³ç»»Äê¡ÊºÞ¡Ë
                   IF     (SPA-COM-INPUTCD (IDX-Z) =   "099999908")
      *H28.9
      *        »ý»²ÌôÍ½Ìó¥³¡¼¥É¤ÏÊñ³çºÞ¤È¤¹¤ë
                    OR    (SPA-COM-INPUTCD (IDX-Z) =   CONS-JSNYCD1
                                                   OR  CONS-JSNYCD2
                                                   OR  CONS-JSNYCD3
                                                   OR  CONS-JSNYCD4
                                                   OR  CONS-JSNYCD5
                                                   OR  CONS-JSNYCD6
                                                   OR  CONS-JSNYCD7
                                                   OR  CONS-JSNYCD8)
      *
                       MOVE    1                   TO  FLG-HKTINP
      *                Á°¹Ô¤âÊñ³ç¤È¤¹¤ë
                       IF      SPA-GMN-INPUTCD (IDX-Z - 1) =   "."
                           MOVE    1           TO  SPA-COM-HOUKATU-FLG
                                                          (IDX-Z - 1)
                           MOVE    ZERO        TO  SPA-COM-HOUKATU-FLG2
                                                          (IDX-Z - 1)
                           MOVE    ZERO        TO  SPA-COM-HOUKATU-KERR
                                                          (IDX-Z - 1)
                       END-IF
                   END-IF
                   IF      FLG-HKTINP          =   1
                       MOVE    1               TO  SPA-COM-HOUKATU-FLG
                                                                (IDX-Z)
                       MOVE    ZERO            TO  SPA-COM-HOUKATU-FLG2
                                                                (IDX-Z)
                       MOVE    ZERO            TO  SPA-COM-HOUKATU-KERR
                                                                (IDX-Z)
                   END-IF
               END-IF
               IF      SPA-COM-ZAIEND (IDX-Z)  =   "1"
                   MOVE    ZERO                TO  FLG-HKTINP
               END-IF
           END-PERFORM
           .
       200213-HKTINPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥É¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       20021-SRYCD-CHECK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK2
           MOVE    ZERO                TO  FLG-CHK3
           PERFORM VARYING     IDX-H    FROM    1   BY  1
                   UNTIL      (IDX-H    >  600   )  OR
                              (TBL-HKTSRYCD(IDX-H) =   SPACE)
               IF      WRK-SRYCD           =   TBL-HKTSRYCD (IDX-H)
                   MOVE    1               TO  FLG-CHK2
                   IF      TBL-SANTEIKBN (IDX-H)   =   "1"
                       MOVE    2               TO  FLG-CHK2
                   END-IF
                   IF      TBL-KENTAIKBN (IDX-H)    =   "1"
                       MOVE    1               TO  FLG-CHK3
                   END-IF
                   MOVE    600             TO  IDX-H
               END-IF
           END-PERFORM
      *
      *    ³°Íè¿ÇÎÅÎÁ¤Ç¾å¤Î¹Ô¤¬¸¡ÂÎ¥³¡¼¥ÉÇ¢¤Î¾ì¹ç¤Î¤ßÂÐ¾Ý
           IF      FLG-CHK3            =   1
               IF    (WRK-KANRICD          =   "001")  AND
                     (FLG-CHK2             =   1    )
                   IF     (IDX-Z               >   1  )  AND
                          (SPA-COM-INPUTCD (IDX-Z - 1 )
                                               =   "160138910" )
                       MOVE    1               TO  FLG-CHK2
                   ELSE
                       MOVE    ZERO            TO  FLG-CHK2
                   END-IF
               ELSE
                   MOVE    ZERO                TO  FLG-CHK3
               END-IF
           END-IF
      *
      *    Êñ³ç¥³¡¼¥É
           IF      FLG-CHK2            =   1
               IF      WRK-SANTEI-HOU      =   "1"
      *            ²ñ·×¥Ç¡¼¥¿ºîÀ®
                   MOVE    1           TO  SPA-COM-HOUKATU-KFLG(IDX-Z)
               ELSE
      *            ¥¨¥é¡¼
                   MOVE    1           TO  SPA-COM-HOUKATU-KERR(IDX-Z)
               END-IF
           END-IF
      *
           IF      FLG-CHK2            =   2
               MOVE    ZERO            TO  SPA-COM-HOUKATU-KFLG(IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-KERR(IDX-Z)
      *        ²Ã»»¤Î¾ì¹ç
               IF     (SPA-COM-DATAKBN (IDX-Z)     =   "2" )
                  AND (SPA-COM-AUTOKBN (IDX-Z) NOT =   "2" )
                   MOVE    1           TO  SPA-COM-HOUKATU-KFLG2(IDX-Z)
               END-IF
           END-IF
      *
           .
       20021-SRYCD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¶èÊ¬¤Ë¤è¤ëÊñ³ç½èÍý
      *****************************************************************
       20022-SRYKBN-CHECK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK3
      *    ÅÀ¿ô¥¼¥í¤ÏÊñ³ç³°
      *    IF      SPA-COM-TENSU  (IDX-Z)    =   ZERO
      *        CONTINUE
      *    ELSE
      *
           EVALUATE    SPA-COM-SRYKBN (IDX-Z)(1:1)
      *        ÅêÌô
               WHEN    "2"
      *            ±¡³°
                   IF     (SPA-COM-SRYSYUKBN(IDX-Z)  =   "212"
                                                     OR  "213"
                                                     OR  "222"
                                                     OR  "223"
                                                     OR  "232"
                                                     OR  "233")
                     OR  ((SPA-COM-TENSU  (IDX-Z)    =   ZERO )  AND
                          (FLG-INPUTOK2              =   ZERO))
      *H28.4           ±¡Æâ¤ÏÅÀ¿ô¥¼¥í¤âÊñ³çÂÐ¾Ý
                       IF    (SPA-COM-SRYSYUKBN(IDX-Z)   =   "231"
                                                         OR  "211"
                                                         OR  "291"
                                                         OR  "221"
                                                         OR  "297"
                                                         OR  "294")
                           IF      WRK-HKTKBN20    =   1
                               MOVE    1           TO  FLG-CHK3
                           END-IF
                       END-IF
                   ELSE
                       IF      WRK-HKTKBN20    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
               WHEN    "3"
                   IF      WRK-HKTKBN30    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "4"
                   IF      WRK-HKTKBN40    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "5"
                   IF      WRK-HKTKBN50    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "6"
                   IF      SPA-COM-SRYKBN (IDX-Z)      =   "60"
                       IF      WRK-HKTKBN60    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
                   IF      SPA-COM-SRYKBN (IDX-Z)      =   "64"
                       IF      WRK-HKTKBN64    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
               WHEN    "7"
                   IF      WRK-HKTKBN70    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "8"
      *            ½èÊý¤»¤óÎÁ¡ÊÅêÌô¡Ë
                   IF      SPA-COM-SRYSYUKBN (IDX-Z)   =   "820"
                       IF      WRK-HKTKBN20    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
      *            ¥ê¥Ï¥Ó¥ê¡Ê¤½¤ÎÂ¾¡Ë
                   IF      SPA-COM-SRYSYUKBN (IDX-Z)   =   "800"
                       IF      WRK-HKTKBN80    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
      *            Àº¿À²ÊÀìÌç
                   IF      SPA-COM-SRYSYUKBN (IDX-Z)   =   "830"
                       IF      WRK-HKTKBN81    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
      *            Êü¼ÍÀþ¼£ÎÅ
                   IF      SPA-COM-SRYSYUKBN (IDX-Z)   =   "840"
                       IF      WRK-HKTKBN82    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
               WHEN    "1"
                   IF      SPA-COM-SRYKBN (IDX-Z)      =   "13"
      *            »ØÆ³ÎÁ
                       IF      WRK-HKTKBN13    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
                   IF      SPA-COM-SRYKBN (IDX-Z)      =   "14"
      *            ºßÂðÎÁ
                       IF      SPA-COM-SRYSYUKBN (IDX-Z)   =   "147"
                                                           OR  "148"
                           CONTINUE
                       ELSE
                           IF      WRK-HKTKBN14    =   1
                               MOVE    1           TO  FLG-CHK3
                           END-IF
                       END-IF
                   END-IF
      *            ½é¿Ç¡¦ºÆ¿Ç¡¦Æþ±¡
                   IF      SPA-COM-SRYKBN (IDX-Z)      =   "11"
                                                       OR  "12"
                                                       OR  "90"
                                                       OR  "92"
                       IF      WRK-HKTKBN10    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
           END-EVALUATE
      *
      ***  END-IF
      *
      *    Êñ³ç¥³¡¼¥É
           IF      FLG-CHK3            =   1
      *        ºÞ¤ò¤¹¤Ù¤Æ
               PERFORM VARYING     IDX-Z2  FROM    IDX-Z  BY  -1
                       UNTIL      (IDX-Z2  <   1   )
                   IF     (IDX-Z           NOT =   IDX-Z2  )  AND
                          (SPA-COM-ZAIEND (IDX-Z2) =   "1" )
                       MOVE    1                   TO  IDX-Z2
                   ELSE
                       IF      WRK-SANTEI-HOU      =   "1"
      *                ²ñ·×¥Ç¡¼¥¿ºîÀ®
                           MOVE    1           TO  SPA-COM-HOUKATU-KFLG
                                                           (IDX-Z2)
                       ELSE
      *                ¥¨¥é¡¼
                          MOVE    1           TO  SPA-COM-HOUKATU-KERR
                                                            (IDX-Z2)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       20022-SRYKBN-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÅ»ÒÅÀ¿ôÉ½¡¡Êñ³ç¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       20028-ETENSU-CHECK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK2
           MOVE    ZERO                TO  FLG-CHK3
      *
      *    ¿ÇÎÅ¶èÊ¬¤ÇÊñ³ç¤Î»þ¤Î¤ß
           INITIALIZE                  ORCSCETNSCHKAREA
           MOVE    "1"                 TO  ORCSCETNSCHK-SYORI
           MOVE    WRK-SRYYMD          TO  ORCSCETNSCHK-SRYYMD
           MOVE    WRK-K-SRYCD         TO  ORCSCETNSCHK-SRYCD1
           MOVE    WRK-SRYCD           TO  ORCSCETNSCHK-SRYCD2
           CALL    "ORCSCETNSCHK"      USING
                                       ORCSCETNSCHKAREA
                                       SPA-AREA
           IF      ORCSCETNSCHK-CHKKBN =   ZERO
      *        Êñ³çÂÐ¾Ý³°
               MOVE    2                   TO  FLG-CHK2
           END-IF
           IF      ORCSCETNSCHK-CHKKBN =   1
      *        Êñ³ç¤¢¤ê
               MOVE    1                   TO  FLG-CHK2
           END-IF
           IF      ORCSCETNSCHK-KNT-SRYCD-G    NOT =   SPACE
      *         ¸¡ÂÎ¥³¡¼¥É¤¢¤ê
               MOVE    1                   TO  FLG-CHK3
           END-IF
      *
      *    ¾å¤Î¹Ô¤¬¸¡ÂÎ¥³¡¼¥É¤Î¾ì¹ç¤Î¤ßÊñ³çÂÐ¾Ý
           IF      FLG-CHK3            =   1
               IF     (IDX-Z               >   1  )
                 AND  (SPA-COM-KENTAICOMMENT (IDX-Z - 1)   =   1 )
      *            ¸¡ÂÎ¥³¡¼¥ÉÈ½Äê
                   PERFORM 200281-ETENSU-SMP-CHK-SEC
               ELSE
                   MOVE    2               TO  FLG-CHK2
               END-IF
           END-IF
      *    Êñ³ç¥³¡¼¥É
           IF      FLG-CHK2            =   1
               IF      WRK-SANTEI-HOU      =   "1"
      *            ²ñ·×¥Ç¡¼¥¿ºîÀ®
                   MOVE    1           TO  SPA-COM-HOUKATU-KFLG(IDX-Z)
               ELSE
      *            ¥¨¥é¡¼
                   MOVE    1           TO  SPA-COM-HOUKATU-KERR(IDX-Z)
               END-IF
           END-IF
      *
      *    Êñ³ç²ò½ü
           IF      FLG-CHK2            =   2
               MOVE    ZERO            TO  SPA-COM-HOUKATU-KFLG(IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-KERR(IDX-Z)
      *        ²Ã»»¤Î¾ì¹ç
               IF     (SPA-COM-DATAKBN (IDX-Z)     =   "2" )
                  AND (SPA-COM-AUTOKBN (IDX-Z) NOT =   "2" )
                   MOVE    1           TO  SPA-COM-HOUKATU-KFLG2(IDX-Z)
               END-IF
           END-IF
      *
           .
       20028-ETENSU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸¡ÂÎ¥³¡¼¥ÉÈ½Äê½èÍý
      *****************************************************************
       200281-ETENSU-SMP-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK4
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL      (IDX-K       >   10    )
                          OR  (ORCSCETNSCHK-KNT-SRYCD (IDX-K)
                                           =   SPACE  )
                          OR  (FLG-CHK4    =   1      )
               IF      SPA-COM-INPUTCD (IDX-Z - 1 )
                                           =   ORCSCETNSCHK-KNT-SRYCD
                                                              (IDX-K)
                   MOVE    1               TO  FLG-CHK4
               END-IF
           END-PERFORM
           IF      FLG-CHK4            =   1
               MOVE    1                   TO  FLG-CHK2
           ELSE
               MOVE    2                   TO  FLG-CHK2
           END-IF
           .
       200281-ETENSU-SMP-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¡Ê³°Íè¡Ë½èÍý
      *****************************************************************
       20013-TANKI3HKT-SEC         SECTION.
      *
      *    Êñ³ç½èÍý
           MOVE    TBL-N-SANTEI-HOU (WRK-TANKI3-IDX)
                                       TO  WRK-SANTEI-HOU
           MOVE    TBL-N-SANTEI-KBN (WRK-TANKI3-IDX)
                                       TO  WRK-SANTEI-KBN
      *
           IF      ORCSCHKTCHK-SYORI   =   "1"
                                       OR  "6"
      *        ÆþÎÏ¹àÌÜ
               PERFORM 200131-TANKI3INPUT-CHK-SEC
           ELSE
      *        ¼«Æ°È¯À¸
               PERFORM 200132-TANKI3AUTO-CHK-SEC
           END-IF
           .
       20013-TANKI3HKT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¡Ê³°Íè¡ËÆþÎÏ¹àÌÜ½èÍý
      *****************************************************************
       200131-TANKI3INPUT-CHK-SEC         SECTION.
      *
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥É¥Á¥§¥Ã¥¯
           MOVE    ZERO                TO  FLG-SANTEIOK
           MOVE    ZERO                TO  FLG-TANKI3HKT-NO
           MOVE    ZERO                TO  FLG-TANKI3HKT-OK
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
               IF     (SPA-COM-HKNCOMBI (IDX-Z)    =   "9999")
                   OR (SPA-COM-HOUKATU-FLG(IDX-Z)  =   1     )
                   OR (SPA-COM-HOUKATU-ERR(IDX-Z)  =   1     )
                   OR (SPA-GMN-INPUTCD (IDX-Z)(1:1)
                                                   =   "#"
                                                   OR  "$"   )
      *            ¼£¸³ÂÐ¾Ý³°
                   OR (SPA-COM-CHIKENKBN(IDX-Z)    =   "1"   )
      *            ÌôÉ¾ÂÐ¾Ý³°
                   OR  (SPA-COM-YAKUHYO (IDX-Z)    =   "1"   )
                   OR  (SPA-COM-GENTEN  (IDX-Z)    =   "1"   )
      *            ¥¢¥Õ¥¿¡¼¥±¥¢
                   OR  (SPA-RSI-HKNKBN             =   "3" )
                   CONTINUE
               ELSE
      *R02.10
      *            £Ð£Ã£Ò¸¡ºº¤ÎÈ½ÃÇÎÁ¤Ï½ÐÍè¹â¤È¤¹¤ë
                   IF     (SPA-COM-INPUTCD(IDX-Z)(1:1)   =   "1"   )
                     AND  (SPA-COM-INPUTCD(IDX-Z) =   WRK-COV-HANDAN1
                                                  OR  WRK-COV-HANDAN2)
                       MOVE    1               TO  FLG-SANTEIOK
                   END-IF
      *
      *            ¸¡ººÈ½ÃÇÎÁ¤Î¤ß¤Î¥Á¥§¥Ã¥¯¤È¤¹¤ë
                   IF     (SPA-COM-INPUTCD (IDX-Z)(1:1) =   "1" )
                   AND    (FLG-SANTEIOK        =   ZERO )
                   AND    (SPA-COM-SRYKBN (IDX-Z)  =   "60"
      *H30.10            ²èÁü¿ÇÃÇÄÉ²Ã
                                                   OR  "70"     )
                   AND    (SPA-COM-DATAKBN(IDX-Z)  =   "1"      )
      *H30.10
                       MOVE    SPA-COM-SRYKBN (IDX-Z)
                                                   TO  WRK-CHK-SRYKBN
                       MOVE    SPA-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
      *                ¥Á¥§¥Ã¥¯
                       PERFORM 200131-TANKI3HKT-CHEK-SEC
                       IF      LNK-TANKI3HKT-KBN   =   "1"
                           IF      WRK-SANTEI-HOU      =   "1"
      *                        ²ñ·×¥Ç¡¼¥¿ºîÀ®
                               MOVE    1       TO  SPA-COM-HOUKATU-KFLG
                                                               (IDX-Z)
                           ELSE
      *                        ¥¨¥é¡¼
                               MOVE    1       TO  SPA-COM-HOUKATU-KERR
                                                               (IDX-Z)
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *        ½ÐÍè¹â»»ÄêÈ½Äê
               IF      SPA-COM-ZAIEND (IDX-Z)  =   "1"
                   MOVE    ZERO                TO  FLG-SANTEIOK
               END-IF
               IF      SPA-COM-INPUTCD (IDX-Z) =   "099999903"
                   MOVE    1                   TO  FLG-SANTEIOK
               END-IF
           END-PERFORM
      *
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥ÉÊÔ½¸
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
               IF     (SPA-COM-HOUKATU-KFLG (IDX-Z)  =   1     )
                   OR (SPA-COM-HOUKATU-KFLG2(IDX-Z)  =   1     )
                   OR (SPA-COM-HOUKATU-KERR (IDX-Z)  =   1     )
                   MOVE    SPA-COM-HOUKATU-KFLG(IDX-Z)
                                       TO  SPA-COM-HOUKATU-FLG (IDX-Z)
                   MOVE    SPA-COM-HOUKATU-KFLG2(IDX-Z)
                                       TO  SPA-COM-HOUKATU-FLG2 (IDX-Z)
                   MOVE    SPA-COM-HOUKATU-KERR(IDX-Z)
                                       TO  SPA-COM-HOUKATU-ERR (IDX-Z)
               END-IF
               MOVE    ZERO                TO  SPA-COM-HOUKATU-KFLG
                                                               (IDX-Z)
               MOVE    ZERO                TO  SPA-COM-HOUKATU-KFLG2
                                                               (IDX-Z)
               MOVE    ZERO                TO  SPA-COM-HOUKATU-KERR
                                                               (IDX-Z)
           END-PERFORM
           .
       200131-TANKI3INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³½èÍý
      *****************************************************************
       200131-TANKI3HKT-CHEK-SEC         SECTION.
      *
      *H30.1O
      *    H30.4¤«¤éÂ¾Æþ±¡ÎÁ¤È¥³¥ó¥Ô¥å¡¼¥¿ÃÇÁØ¿ÇÃÇÅùÂÐ¾Ý
           IF     (WRK-SRYYMD          <   "20180401")
             AND ((WRK-CHK-SRYKBN      =   "70" )
               OR (SPA-NYUGAIKBN       =   "1"  ))
               GO      TO      200131-TANKI3HKT-CHEK-EXT
           END-IF
      *
           INITIALIZE                  ORCSTANKI3HKT-AREA
      *    °åÎÅµ¡´ØÈÖ¹æ
           MOVE    SPA-HOSPNUM         TO  LNK-TANKI3HKT-HOSPNUM
      *    ´µ¼Ô£É£Ä
           MOVE    SPA-PTID            TO  LNK-TANKI3HKT-PTID
      *    »»ÄêÇ¯·îÆü
           MOVE    WRK-SRYYMD          TO  LNK-TANKI3HKT-SANTEIYMD
      *    ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É
           MOVE    WRK-SRYCD           TO  LNK-TANKI3HKT-SRYCD
      *H29.4
           IF     (FLG-TANKI3HKT-OK    =   1  )
              AND (FLG-TANKI3HKT-NO    =   ZERO )
               MOVE    "1"                 TO  LNK-TANKI3HKT-HKTKBN
           END-IF
      *
      *
           CALL    "ORCSTANKI3HKT"     USING
                                       ORCSTANKI3HKT-AREA
                                       SPA-AREA
      *
           IF      LNK-TANKI3HKT-KBN2  =   "1"
               MOVE    1                   TO  FLG-TANKI3HKT-NO
           END-IF
           IF      LNK-TANKI3HKT-KBN   =   "1"
               MOVE    1                   TO  FLG-TANKI3HKT-OK
           END-IF
           .
       200131-TANKI3HKT-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¡Ê³°Íè¡Ë¼«Æ°È¯À¸½èÍý
      *****************************************************************
       200132-TANKI3AUTO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TANKI3HKT-NO
           MOVE    ZERO                TO  FLG-TANKI3HKT-OK
           INITIALIZE                  WRK-ORCSC-KASAN
           PERFORM VARYING     IDX-L1  FROM    1   BY  1
                   UNTIL      (IDX-L1      >   10  )
               PERFORM VARYING     IDX-L2      FROM    1   BY  1
                       UNTIL      (IDX-L2      >   30      )
                   IF     (LNK-ORCSC-SRYCD (IDX-L1 IDX-L2)
                                           NOT = SPACE     )
      *                ¼£¸³ÂÐ¾Ý³°
                   AND    (LNK-ORCSC-CHIKENKBN (IDX-L1 IDX-L2)
                                               =   SPACE   )
      *            ¥¢¥Õ¥¿¡¼¥±¥¢
                   AND    (SPA-RSI-HKNKBN      NOT =   "3" )
                   AND    (LNK-ORCSC-HKTKBN  (IDX-L1 IDX-L2)
                                                   =   ZERO)
      *                ¿ÇÎÅ¥³¡¼¥ÉÊñ³ç
                       MOVE    LNK-ORCSC-SRYCD (IDX-L1 IDX-L2)
                                                   TO  WRK-SRYCD
      *R02.10
      *                £Ð£Ã£Ò¸¡ºº¤¢¤ê¤ÏÈ½ÃÇÎÁ»»Äê²Ä
                       IF     (WRK-SRYCD(1:1)      =   "1"   )
                          AND (WRK-SRYCD           =   WRK-COV-HANDAN1
                                                   OR  WRK-COV-HANDAN2)
      *                    ½ÐÍè¹â¤È¤¹¤ë
                           MOVE    ZERO            TO  WRK-ORCSC-HKTKBN
                                                  (IDX-L1 IDX-L2)
                          CONTINUE
                       ELSE
      *!!!
      *                ¼êµ»ÎÁ¤Î¤ß¥Á¥§¥Ã¥¯
                       IF     (WRK-SRYCD (1:1)     =   "1" )
                         AND  (LNK-ORCSC-SRYKBN (IDX-L1 IDX-L2)
                                                   =   "60"
      *H30.10            ²èÁü¿ÇÃÇÄÉ²Ã
                                                   OR  "70")
      *
      *H30.10
                           MOVE    LNK-ORCSC-SRYKBN (IDX-L1 IDX-L2)
                                                   TO  WRK-CHK-SRYKBN
      *                    ¥Á¥§¥Ã¥¯
                           PERFORM 200131-TANKI3HKT-CHEK-SEC
                           IF      LNK-TANKI3HKT-KBN   =   "1"
                               IF      WRK-SANTEI-HOU      =   "1"
      *                         ²ñ·×¥Ç¡¼¥¿ºîÀ®
                                   MOVE    1       TO  WRK-ORCSC-HKTKBN
                                                      (IDX-L1 IDX-L2)
                               ELSE
      *                        ¥¨¥é¡¼¡Êºï½ü¡Ë
                                   MOVE    SPACE   TO  LNK-ORCSC-SRYCD
                                                      (IDX-L1 IDX-L2)
                               END-IF
      *
                          END-IF
                       END-IF
      *R02.10
                     END-IF
      *
                   END-IF
               END-PERFORM
           END-PERFORM
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥ÉÊÔ½¸
           PERFORM VARYING     IDX-L1  FROM    1   BY  1
                   UNTIL      (IDX-L1      >   10  )
               PERFORM VARYING     IDX-L2      FROM    1   BY  1
                       UNTIL      (IDX-L2      >   30      )
                   IF     (LNK-ORCSC-SRYCD  (IDX-L1 IDX-L2)
                                               NOT =   SPACE )  AND
                          (WRK-ORCSC-HKTKBN (IDX-L1 IDX-L2)
                                                   =   1     )
                       MOVE    WRK-ORCSC-HKTKBN (IDX-L1 IDX-L2)
                                           TO  LNK-ORCSC-HKTKBN
                                                       (IDX-L1 IDX-L2)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       200132-TANKI3AUTO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«Æ°È¯À¸Ê¬¤Î¤ß½èÍý
      *****************************************************************
       300-AUTO-SYORI-SEC         SECTION.
      *
      *    ½ÐÍè¹âÈ½Äê
           MOVE    ZERO                TO  FLG-DEKIDAKA
      *R02.10
      *    £Ð£Ã£Ò¸¡ºº¤ÎÍ­Ìµ
           MOVE    ZERO                TO  FLG-SARSCOV
           MOVE    SPACE               TO  WRK-COV-HANDAN1
           MOVE    SPACE               TO  WRK-COV-HANDAN2
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  ) 
      *        ½ÐÍè¹â¡Ê²ñ·×¡Ë¤¬¤¢¤ì¤Ð¡¢¤¹¤Ù¤Æ½ÐÍè¹â
               IF      SPA-COM-INPUTCD (IDX-Z) =   "099999905"
                   MOVE    1                   TO  FLG-DEKIDAKA
               END-IF
      *R02.10
      *        ½ÐÍè¹â¤Î£Ð£Ã£Ò¸¡ººÈ½Äê
               IF     (SPA-COM-HOUKATU-FLG(IDX-Z)  =   ZERO )
                 AND  (SPA-COM-HOUKATU-FLG2(IDX-Z) =   ZERO )
                 AND  (SPA-COM-HOUKATU-ERR (IDX-Z) =   ZERO )
                 AND  (SPA-COM-INPUTCD(IDX-Z)(1:1) =   "1"  )
                 AND  ((SPA-SRYYMD          >=  "20200901") AND
                       (SPA-SRYYMD          <   "20240601"))
                   PERFORM 200214-PCRKENSA-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           IF      FLG-DEKIDAKA        =   ZERO
      *    Êñ³ç¥Á¥§¥Ã¥¯È½Äê
           IF      SPA-HKTSANTEI-FLG   =   1
               PERFORM 2001-TAISYOU-CHK-SEC
           END-IF
      *    Æþ±¡ÎÁÊñ³ç¥Á¥§¥Ã¥¯È½Äê
           IF      ORCSCHKTCHK-NYUHKTCHK   =   1
               PERFORM 600-NYUHKT-SYORI-SEC
           END-IF
      *
           END-IF
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«Æ°È¯À¸¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       30021-AUTO-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-ORCSC-KASAN
           PERFORM VARYING     IDX-L1  FROM    1   BY  1
                   UNTIL      (IDX-L1      >   10  )
               PERFORM VARYING     IDX-L2      FROM    1   BY  1
                       UNTIL      (IDX-L2      >   30      )
                   IF     (LNK-ORCSC-SRYCD (IDX-L1 IDX-L2)
                                           NOT = SPACE     )
      *                ¼£¸³ÂÐ¾Ý³°
                   AND    (LNK-ORCSC-CHIKENKBN (IDX-L1 IDX-L2)
                                               =   SPACE   )
      *            ¥¢¥Õ¥¿¡¼¥±¥¢
                   AND    (SPA-RSI-HKNKBN      NOT =   "3" )
      *                ¿ÇÎÅ¶èÊ¬¤Ë¤è¤ëÊñ³ç
                       PERFORM 30022-SRYKBN-CHECK-SEC
      *                ¿ÇÎÅ¥³¡¼¥ÉÊñ³ç
                       MOVE    LNK-ORCSC-SRYCD (IDX-L1 IDX-L2)
                                                   TO  WRK-SRYCD
      *R02.10
      *                £Ð£Ã£Ò¸¡ºº¤¢¤ê¤ÏÈ½ÃÇÎÁ»»Äê²Ä
                       IF     (WRK-SRYCD(1:1)      =   "1"     )
                         AND  (WRK-SRYCD           =   WRK-COV-HANDAN1
                                                   OR  WRK-COV-HANDAN2)
      *                    ½ÐÍè¹â¤È¤¹¤ë
                           MOVE    ZERO            TO  WRK-ORCSC-HKTKBN 
                                                  (IDX-L1 IDX-L2)
                          CONTINUE
                       ELSE
      *!
      *                ¼êµ»ÎÁ¤Î¤ß¥Á¥§¥Ã¥¯
                       IF      WRK-SRYCD (1:1)     =   "1"
      *
                       IF      TBL-HKTSRYCD (1)    =   SPACE
      *                    ÅÅ»ÒÅÀ¿ôÉ½¤«¤é¤Î¥Á¥§¥Ã¥¯
                           PERFORM 30022-ETENSU-CHECK-SEC
                       ELSE
      *                    Êñ³ç¥Á¥§¥Ã¥¯¥Þ¥¹¥¿¤«¤é¤Î¥Á¥§¥Ã¥¯
                           PERFORM 30021-SRYCD-CHECK-SEC
                       END-IF
      *
                       END-IF
      *!
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥ÉÊÔ½¸
           PERFORM VARYING     IDX-L1  FROM    1   BY  1
                   UNTIL      (IDX-L1      >   10  )
               PERFORM VARYING     IDX-L2      FROM    1   BY  1
                       UNTIL      (IDX-L2      >   30      )
                   IF     (LNK-ORCSC-SRYCD  (IDX-L1 IDX-L2)
                                               NOT =   SPACE )  AND
                          (WRK-ORCSC-HKTKBN (IDX-L1 IDX-L2)
                                                   =   1     )
                       MOVE    WRK-ORCSC-HKTKBN (IDX-L1 IDX-L2)
                                           TO  LNK-ORCSC-HKTKBN
                                                       (IDX-L1 IDX-L2)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       30021-AUTO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¶èÊ¬¤Ë¤è¤ëÊñ³ç½èÍý
      *****************************************************************
       30022-SRYKBN-CHECK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK3
           EVALUATE    LNK-ORCSC-SRYKBN (IDX-L1 IDX-L2)(1:1)
      *        ÅêÌô¡Ê³°Íè¤Ï¥Á¥§¥Ã¥¯¤Ê¤·¡Ë
               WHEN    "2"
                   IF      WRK-HKTKBN20    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "3"
                   IF      WRK-HKTKBN30    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "4"
                   IF      WRK-HKTKBN40    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "5"
                   IF      WRK-HKTKBN50    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "6"
                   IF      LNK-ORCSC-SRYKBN(IDX-L1 IDX-L2) =   "60"
                       IF      WRK-HKTKBN60    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
                   IF      LNK-ORCSC-SRYKBN(IDX-L1 IDX-L2) =   "64"
                       IF      WRK-HKTKBN64    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
               WHEN    "7"
                   IF      WRK-HKTKBN70    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "8"
      *            ½èÊý¤»¤óÎÁ¡ÊÅêÌô¡Ë
                   IF      LNK-ORCSC-SRYSYUKBN(IDX-L1 IDX-L2)
                                                   =   "820"
                       IF      WRK-HKTKBN20    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
                   IF      LNK-ORCSC-SRYSYUKBN(IDX-L1 IDX-L2)
                                                   =   "800"
                       IF      WRK-HKTKBN80    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
                   IF      LNK-ORCSC-SRYSYUKBN(IDX-L1 IDX-L2)
                                                   =   "830"
                       IF      WRK-HKTKBN81    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
                   IF      LNK-ORCSC-SRYSYUKBN(IDX-L1 IDX-L2)
                                                   =   "840"
                       IF      WRK-HKTKBN82    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
               WHEN    "1"
                   IF      LNK-ORCSC-SRYKBN(IDX-L1 IDX-L2) =   "13"
      *            »ØÆ³ÎÁ
                       IF      WRK-HKTKBN13    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
                   IF      LNK-ORCSC-SRYKBN(IDX-L1 IDX-L2) =   "14"
      *            ºßÂðÎÁ
                       IF      WRK-HKTKBN14    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
      *            ½é¿Ç¡¦ºÆ¿Ç¡¦Æþ±¡
                   IF      LNK-ORCSC-SRYKBN(IDX-L1 IDX-L2) =   "11"
                                                       OR  "12"
                                                       OR  "90"
                                                       OR  "92"
                       IF      WRK-HKTKBN10    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    ³°Íè¿ÇÎÅÎÁ¤Ï¼«Æ°È¯À¸¤·¤¿È½ÃÇÎÁ¤ò»»Äê
           IF    (WRK-KANRICD          =   "001")
               MOVE    ZERO                TO  FLG-CHK3
           END-IF
      *
           IF      FLG-CHK3            =   1
               IF      WRK-SANTEI-HOU      =   "1"
      *                ²ñ·×¥Ç¡¼¥¿ºîÀ®
                   MOVE    1           TO  WRK-ORCSC-HKTKBN
                                                  (IDX-L1 IDX-L2)
               ELSE
      *                ¥¨¥é¡¼¡Êºï½ü¡Ë
                   MOVE    2           TO  WRK-ORCSC-HKTKBN
                                                  (IDX-L1 IDX-L2)
               END-IF
           END-IF
      *
           .
       30022-SRYKBN-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¥³¡¼¥É¤Ë¤è¤ëÊñ³ç½èÍý
      *****************************************************************
       30021-SRYCD-CHECK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK2
           MOVE    ZERO                TO  FLG-CHK3
      *
           PERFORM VARYING     IDX-H    FROM    1   BY  1
                   UNTIL      (IDX-H    >  600   )  OR
                              (TBL-HKTSRYCD(IDX-H) =   SPACE)
               IF      WRK-SRYCD           =   TBL-HKTSRYCD (IDX-H)
                   MOVE    1               TO  FLG-CHK2
                   IF      TBL-SANTEIKBN (IDX-H)   =   "1"
                       MOVE    2               TO  FLG-CHK2
                   END-IF
                   IF      TBL-KENTAIKBN (IDX-H)    =   "1"
                       MOVE    1               TO  FLG-CHK3
                   END-IF
                   MOVE    600             TO  IDX-H
               END-IF
           END-PERFORM
      *
      *    ³°Íè¿ÇÎÅÎÁ¤Ç¾å¤Î¹Ô¤¬¸¡ÂÎ¥³¡¼¥ÉÇ¢¤Î¾ì¹ç¤Î¤ßÂÐ¾Ý
           IF      FLG-CHK3            =   1
               IF    (WRK-KANRICD          =   "001")  AND
                     (FLG-CHK2             =   1    )
                   IF     (IDX-Z               >   1  )  AND
                          (SPA-COM-INPUTCD (IDX-Z - 1 )
                                               =   "160138910" )
                       MOVE    1               TO  FLG-CHK2
                   ELSE
                       MOVE    ZERO            TO  FLG-CHK2
                   END-IF
               ELSE
                   MOVE    ZERO                TO  FLG-CHK3
               END-IF
           END-IF
      *
      *    Êñ³ç¥³¡¼¥É
           IF      FLG-CHK2            =   2
      *        Êñ³çÂÐ¾Ý³°
               MOVE    ZERO            TO  WRK-ORCSC-HKTKBN 
                                                  (IDX-L1 IDX-L2)
           ELSE
               IF     (FLG-CHK2            =   1  )
                   IF      WRK-SANTEI-HOU      =   "1"
      *                ²ñ·×¥Ç¡¼¥¿ºîÀ®
                       MOVE    1           TO  WRK-ORCSC-HKTKBN 
                                                  (IDX-L1 IDX-L2)
                   ELSE
      *                ¥¨¥é¡¼¡Êºï½ü¡Ë
                       MOVE    SPACE       TO  LNK-ORCSC-SRYCD
                                                  (IDX-L1 IDX-L2)
                   END-IF
               END-IF
      *        ¿ÇÎÅ¶èÊ¬¤Î¥¨¥é¡¼¤Îºï½ü
               IF     (WRK-ORCSC-HKTKBN (IDX-L1 IDX-L2)
                                           =   2  )
      *            ¥¨¥é¡¼¡Êºï½ü¡Ë
                   MOVE    SPACE       TO  LNK-ORCSC-SRYCD
                                                  (IDX-L1 IDX-L2)
               END-IF
           END-IF
           .
       30021-SRYCD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÅ»ÒÅÀ¿ôÉ½¡¡Êñ³ç¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       30022-ETENSU-CHECK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK2
           MOVE    ZERO                TO  FLG-CHK3
      *
      *    ¿ÇÎÅ¶èÊ¬¤ÇÊñ³ç¤Î»þ¤Î¤ß
           INITIALIZE                  ORCSCETNSCHKAREA
           MOVE    "1"                 TO  ORCSCETNSCHK-SYORI
           MOVE    WRK-SRYYMD          TO  ORCSCETNSCHK-SRYYMD
           MOVE    WRK-K-SRYCD         TO  ORCSCETNSCHK-SRYCD1
           MOVE    WRK-SRYCD           TO  ORCSCETNSCHK-SRYCD2
           CALL    "ORCSCETNSCHK"      USING
                                       ORCSCETNSCHKAREA
                                       SPA-AREA
           IF      ORCSCETNSCHK-CHKKBN =   ZERO
               MOVE    2                   TO  FLG-CHK2
           END-IF
           IF      ORCSCETNSCHK-CHKKBN =   1
               MOVE    1                   TO  FLG-CHK2
           END-IF
      *
      *    Êñ³ç¥³¡¼¥É
           IF      FLG-CHK2            =   2
      *        Êñ³çÂÐ¾Ý³°
               MOVE    ZERO            TO  WRK-ORCSC-HKTKBN 
                                                  (IDX-L1 IDX-L2)
           ELSE
               IF     (FLG-CHK2            =   1  )
                   IF      WRK-SANTEI-HOU      =   "1"
      *                ²ñ·×¥Ç¡¼¥¿ºîÀ®
                       MOVE    1           TO  WRK-ORCSC-HKTKBN 
                                                  (IDX-L1 IDX-L2)
                   ELSE
      *                ¥¨¥é¡¼¡Êºï½ü¡Ë
                       MOVE    SPACE       TO  LNK-ORCSC-SRYCD
                                                  (IDX-L1 IDX-L2)
                   END-IF
               END-IF
      *        ¿ÇÎÅ¶èÊ¬¤Î¥¨¥é¡¼¤Îºï½ü
               IF     (WRK-ORCSC-HKTKBN (IDX-L1 IDX-L2)
                                           =   2  )
      *            ¥¨¥é¡¼¡Êºï½ü¡Ë
                   MOVE    SPACE       TO  LNK-ORCSC-SRYCD
                                                  (IDX-L1 IDX-L2)
               END-IF
           END-IF
      *
           .
       30022-ETENSU-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    ¥¨¥é¡¼ºï½ü½èÍý
      *****************************************************************
       400-ERR-DELETE-SEC         SECTION.
      *
      *2024/04
      *    ¥³¥á¥ó¥È¤Î¤ß¤Ë¤Ê¤Ã¤¿¾ì¹ç¤Ïºï½üÂÐ¾Ý¤È¤¹¤ë
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  FLG-ERRGAI
           MOVE    ZERO                TO  FLG-ERRARI
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                             ((SPA-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDX-Z) =   SPACE ))
               IF     (SPA-COM-HOUKATU-ERR (IDX-Z) =   1  )
                   MOVE    1               TO  FLG-ERRARI
               ELSE
      *            Êñ³ç³°¤¢¤ê
                   IF      SPA-COM-INPUTCD (IDX-Z)(1:1)  =   "1"
                                                         OR  "6"
                                                         OR  "7"
                       MOVE    1               TO  FLG-ERRGAI
                   END-IF
               END-IF
               IF     (SPA-COM-ZAIEND (IDX-Z)  =   "1" )
                   IF    (FLG-ERRARI          =   1  )
                     AND (FLG-ERRGAI          =   ZERO)
      *                ºÞ¤¹¤Ù¤Æ¤òºï½üÂÐ¾Ý¤È¤¹¤ë
                       ADD     1              TO  IDX-S
                       PERFORM VARYING    IDX-Z2  FROM   IDX-S  BY  1
                               UNTIL      IDX-Z2  >   IDX-Z
                          MOVE    1       TO  SPA-COM-HOUKATU-ERR
                                                        (IDX-Z2)
                       END-PERFORM
                   END-IF
                   MOVE    IDX-Z          TO  IDX-S
                   MOVE    ZERO           TO  FLG-ERRARI
                   MOVE    ZERO           TO  FLG-ERRGAI
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                             ((SPA-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDX-Z) =   SPACE ))
               IF     (SPA-COM-HOUKATU-ERR (IDX-Z) =   1  )
                   MOVE    SPACE           TO  SPA-GMN-INPUTCD (IDX-Z)
      *2024/04
      *            Á°¹Ô¤Ç½ªÎ»¤Î»þ
                   IF     (IDX-Z           >   1  )
                     AND  (SPA-COM-ZAIEND (IDX-Z - 1 ) =   "1" )
      *              ¿ÇÎÅ¼ïÊÌ¶èÊ¬¤òÄÉ²Ã
                     IF     (SPA-COM-INPUTCD (IDX-Z)(1:1)  =   "1" )
                       AND  (SPA-COM-ZAIEND  (IDX-Z)   NOT =   "1" )
                       MOVE    SPA-COM-TNSSRYSYUKBN (IDX-Z)
                                               TO  WRK-G-SRYSYUKBN
                       INITIALIZE              SPA-COM-TBLREC  (IDX-Z)
                       INITIALIZE              SPA-GMN-TBLREC  (IDX-Z)
                       MOVE    "."             TO  SPA-GMN-INPUTCD
                                                        (IDX-Z)(1:1)
                       MOVE    WRK-G-SRYSYUKBN
                                                TO  SPA-GMN-INPUTCD
                                                         (IDX-Z)(2:3)
                     END-IF
                   END-IF
               END-IF
           END-PERFORM
      *****
      *    ¿ÇÎÅ¼ïÊÌ¤À¤±¤ÎºÞ¤Ïºï½ü
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                             ((SPA-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDX-Z) =   SPACE ))
               IF     (SPA-GMN-INPUTCD (IDX-Z) (1:1)   =   "." )
                   COMPUTE IDX-Z3              =   IDX-Z   +  1
                   MOVE    ZERO                TO  FLG-ZAIARI
                   PERFORM VARYING     IDX-Z2  FROM    IDX-Z3  BY  1
                           UNTIL      (IDX-Z2      >   SPA-MAX-LINE)
                       IF      (SPA-GMN-INPUTCD (IDX-Z2)(1:1)
                                                   NOT =   "." ) AND
                               (SPA-GMN-INPUTCD (IDX-Z2) NOT =   SPACE)
                           MOVE    1               TO  FLG-ZAIARI
                       END-IF
                       IF      (SPA-COM-ZAIEND (IDX-Z2)    =   "1"  )
                           MOVE    SPA-MAX-LINE        TO  IDX-Z2
                       END-IF
                   END-PERFORM
                   IF      FLG-ZAIARI          =   ZERO
                       MOVE    SPACE       TO  SPA-GMN-INPUTCD (IDX-Z)
                   END-IF
               END-IF
           END-PERFORM
           .
       400-ERR-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁ»»ÄêÈ½Äê½èÍý
      *****************************************************************
       500-NYUIN-CHKSYORI-SEC         SECTION.
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ
           INITIALIZE                      ORCSNYUINDAY-AREA
           MOVE    SPA-HOSPNUM         TO  LNK-NYUINDAY-HOSPNUM
           MOVE    SPA-PTID            TO  LNK-NYUINDAY-PTID
           MOVE    WRK-SRYYM           TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING
                                           ORCSNYUINDAY-AREA
                                           SPA-AREA
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    WRK-SRYYM           TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *    Æþ±¡ÎÁ»»Äê¤Ê¤·
           IF      FLG-NYUINACCT       =   1
               MOVE    1                   TO  ORCSCHKTCHK-NYUERR
           ELSE
               MOVE    ZERO                TO  ORCSCHKTCHK-NYUERR
           END-IF
      *
           PERFORM UNTIL      (FLG-NYUINACCT     =   1)
      *
               PERFORM 5001-NYUINACT-SYORI-SEC
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ¿ÇÎÅÆü»þÅÀ¤ÎÆþ±¡ÎÁ¤ÈÆ±¤¸Æü¤Î¤ßÂÐ¾Ý¤È¤¹¤ë
           MOVE    WRK-SRYDD           TO  IDX-D
           MOVE    SPACE               TO  ORCSCHKTCHK-NYUHKTCD
           MOVE    SPACE               TO  ORCSCHKTCHK-NYUSRYCD
           IF      WRK-NYUHKTCD (IDX-D)    NOT =   "XXX"
               MOVE    WRK-NYUHKTCD (IDX-D)    TO  ORCSCHKTCHK-NYUHKTCD
               MOVE    WRK-N-SRYCD  (IDX-D)    TO  ORCSCHKTCHK-NYUSRYCD
           END-IF
      *    Êñ³ç
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
               IF      WRK-NYUHKTCD (IDX)  =   WRK-NYUHKTCD (IDX-D)
                   MOVE    1               TO  ORCSCHKTCHK-NYUHKTFLG
                                                              (IDX)
               ELSE
                   MOVE    ZERO            TO  ORCSCHKTCHK-NYUHKTFLG
                                                              (IDX)
               END-IF
               IF      WRK-NYUHKTCD (IDX)  =   SPACE
                                           OR  "XXX"
                   CONTINUE
               ELSE
      *            ·îÆâ¤ËÊñ³çÆþ±¡ÎÁ¤¬¤¢¤ë»þ
                   MOVE    1               TO  ORCSCHKTCHK-NYUHKTCHK
               END-IF
           END-PERFORM
      *
      *    ½ÐÍè¹â»»ÄêÆü¸¡º÷
           PERFORM 5009-SANTEI-CHK-SEC
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
               IF      TBL-DEKIDAKA (IDX)  =   1
      *            ½ÐÍè¹â»»Äê¥³¡¼¥É»»Äê
                   MOVE    1               TO  ORCSCHKTCHK-DEKIDAFLG
                                                              (IDX)
               END-IF
           END-PERFORM
      *!!!
      *H29.1
      *    Æþ±¡Ä´ºÞÎÁ¤Î°Ù¡¢Íâ·î¤ÎÆþ±¡ÎÁÈ½Äê
           MOVE    ZERO                TO  ORCSCHKTCHK-NYUERR-2
           IF     (ORCSCHKTCHK-NYUERR      =   ZERO)
              AND (ORCSCHKTCHK-NYUHKTCHK   =   1  )
              PERFORM 5009-NEXTYMD-NYUINCHEK-SEC
           END-IF
           .
       500-NYUIN-CHKSYORI-EXT.
           EXIT.
      *
      *H29.1
      *****************************************************************
      *    Íâ·î¤ÎÆþ±¡ÎÁ¤ÎÈ½Äê½èÍý
      *****************************************************************
       5009-NEXTYMD-NYUINCHEK-SEC         SECTION.
      *
      *    ·îËöÆü¤òµá¤á¤ë
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-SRYYM           TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI(7:2)    TO  WRK-LST-DD
           ELSE
               MOVE    31                  TO  WRK-LST-DD
           END-IF
           IF     (WRK-TANKI-DAY   NOT =   ZERO )
             AND  (WRK-TANKI-DAY       =   WRK-LST-DD )
               GO    TO   5009-NEXTYMD-NYUINCHEK-EXT
           END-IF
      *
           MOVE    WRK-SRYYM           TO  WRK-N-SRYYM
           IF      WRK-N-SRYMM         =   12
               MOVE    1               TO  WRK-N-SRYMM
               ADD     1               TO  WRK-N-SRYYY
           ELSE
               ADD     1               TO  WRK-N-SRYMM
           END-IF
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ
           INITIALIZE                      ORCSNYUINDAY-AREA
           MOVE    SPA-HOSPNUM         TO  LNK-NYUINDAY-HOSPNUM
           MOVE    SPA-PTID            TO  LNK-NYUINDAY-PTID
           MOVE    WRK-N-SRYYM         TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING
                                           ORCSNYUINDAY-AREA
                                           SPA-AREA
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    WRK-N-SRYYM         TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *    Æþ±¡ÎÁ»»Äê¤Ê¤·
           IF      FLG-NYUINACCT       =   1
               MOVE    1                   TO  ORCSCHKTCHK-NYUERR-2
           ELSE
               MOVE    ZERO                TO  ORCSCHKTCHK-NYUERR-2
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       5009-NEXTYMD-NYUINCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³çÆþ±¡ÎÁ¤ÎÈ½Äê½èÍý
      *****************************************************************
       5001-NYUINACT-SYORI-SEC         SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACT-RAED-SEC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  WRK-IDX-N
      *R03.4
           MOVE    ZERO                TO  FLG-TANKI2
      *
           PERFORM UNTIL     ( FLG-NYUINACT    =   1 )
               PERFORM VARYING     IDZ     FROM    1  BY  1
                       UNTIL      (IDZ         >   5  )  OR
                                  (NSRY-SRYCD (IDZ) =   SPACE)
                   PERFORM 5001-SRYCD-CHK-SEC
               END-PERFORM
      *
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACT-RAED-SEC
           END-PERFORM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
      *
      *        Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁÈ½Äê
               IF    ((WRK-IDX-N               >   ZERO  ) AND
                      (TBL-N-KANRICD (WRK-IDX-N)   =   "526"
                                                   OR  "525"))
                 AND ( LNK-NYUINDAY-TANKI2-KBN (IDX-D)
                                           NOT =   SPACE     )
                   MOVE    1               TO  NACCT-DAY (IDX-D)
      *
      *H29.1
                   MOVE    IDX-D           TO  WRK-TANKI-DAY
               END-IF
      *R03.4
      *****    Ã»¼ê£²¡¤£³¤¬Êñ³çÂÐ¾Ý³°¤Î»þ¤Î»»ÄêÆüÀßÄê
               IF    (WRK-IDX-N            =   ZERO )
                AND  (NACCT-DAY (IDX-D)    =   ZERO )
                AND  (FLG-TANKI2           =   1   )
                   IF     (LNK-NYUINDAY-TANKI2-KBN(IDX-D) 
                                           NOT =   SPACE)
                      MOVE    1            TO  NACCT-DAY (IDX-D)
                   END-IF
               END-IF
               MOVE    ZERO             TO  FLG-TANKI2
      *****
               IF    ( NACCT-DAY (IDX-D)   NOT =   ZERO )
                   IF      WRK-IDX-N       >   ZERO
                       MOVE    TBL-N-KANRICD (WRK-IDX-N)
                                           TO  WRK-NYUHKTCD (IDX-D)
                       MOVE    WRK-NT-SRYCD
                                           TO  WRK-N-SRYCD (IDX-D)
                   ELSE
                       MOVE    "XXX"       TO  WRK-NYUHKTCD (IDX-D)
      *R03.4
      *****            Êñ³çÂÐ¾Ý³°¤Î»þ
                       IF    (IDX-D       <  31 )
                       AND   (NACCT-DAY (IDX-D + 1) =  ZERO)
      *                ÍâÆü¤¬»»Äê¤Ê¤·¤Ç¡¢Ã»¼ê£²¡¤£³¤Î»»Äê¤¢¤ê
                           IF     (LNK-NYUINDAY-TANKI2-KBN(IDX-D + 1)
                                           NOT =  SPACE)
                              MOVE    1        TO   FLG-TANKI2
                           END-IF
                       END-IF
      *****
                   END-IF
               END-IF
           END-PERFORM
           .
       5001-NYUINACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³çÆþ±¡ÎÁ¤ÎÈ½Äê½èÍý
      *****************************************************************
       5001-SRYCD-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-NT-SRYCD
           PERFORM VARYING     IDX-N   FROM    1  BY  1
                   UNTIL      (IDX-N       >   TBL-N-MAX )
                           OR (FLG-CHK     =   1         )
               PERFORM VARYING     IDY     FROM    1   BY  1
      *R06.6           UNTIL      (IDY     >   200  )  OR
                       UNTIL      (IDY     >   400  )  OR
                                  (TBL-N-SRYCD (IDX-N IDY) =   SPACE)
                   IF      NSRY-SRYCD(IDZ) =   TBL-N-SRYCD (IDX-N IDY)
                       MOVE    TBL-N-SRYCD (IDX-N IDY)
                                                   TO  WRK-NT-SRYCD
                       MOVE    1                   TO  FLG-CHK
                       MOVE    IDX-N               TO  WRK-IDX-N
      ***              MOVE    200                 TO  IDY
                       MOVE    400                 TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       5001-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ÐÍè¹â»»ÄêÆü¸¡º÷¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       5009-SANTEI-CHK-SEC         SECTION.
      *
           INITIALIZE                  TBL-DEKIDAKA-AREA
      *    »»ÄêÍúÎò¸¡º÷
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYYM           TO  SANTEI-SRYYM
           MOVE    "099999904"         TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ÄûÀµ¤Î»þ¡¢ÅöÆü»»ÄêÍ­Ìµ¤ÎÈ½Äê
           MOVE    SANTEI-SRYCD        TO  WRK-SRYCD
           MOVE    WRK-SRYDD           TO  IDX-D
           IF      SPA-SYORIFLG        =   1
               PERFORM VARYING IDZ     FROM    1   BY  1
                       UNTIL   (IDZ        >   SPA-MAX-LINE )  OR
                               (LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   SPACE )
                   IF      LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   WRK-SRYCD
                       MOVE    ZERO            TO  SANTEI-DAY (IDX-D)
                   END-IF
               END-PERFORM
           END-IF
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL       IDD     >   31
               MOVE    SANTEI-DAY (IDD)    TO  TBL-DEKIDAKA (IDD)
           END-PERFORM
      *
           .
       5009-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡´ü´Ö¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       600-NYUINDAYCHK-SEC         SECTION.
      *
      *    ½ÐÍè¹â»»ÄêÆü¥³¡¼¥ÉÈ½Äê
           INITIALIZE                  TBL-DEKIDAKA-AREA
           PERFORM VARYING IDX-N       FROM    1   BY  1
                   UNTIL  (IDX-N       >   31  )
               MOVE    ORCSCHKTCHK-DEKIDAFLG (IDX-N)
                                           TO  TBL-DEKIDAKA  (IDX-N)
           END-PERFORM
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
               IF      SPA-COM-INPUTCD  (IDX-Z)    =   "099999904"
                   PERFORM VARYING IDX-N       FROM    1   BY  1
                           UNTIL  (IDX-N       >   31  )
                       IF      SPA-COM-NYUINDAY(IDX-Z IDX-N)
                                                   >   ZERO
                           MOVE    1           TO  TBL-DEKIDAKA  (IDX-N)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           MOVE    WRK-SRYDD           TO  IDX-D
           IF      ORCSCHKTCHK-NYUHKTCD    NOT =   SPACE
      *        ¿ÇÎÅÆü¤¬Æþ±¡ÎÁÊñ³ç¤Ç½ÐÍè¹â»»ÄêÆü¤Î»þ¡¢½ÐÍè¹â¤È¤¹¤ë
               IF     (TBL-DEKIDAKA  (IDX-D)   =   1  )
                   MOVE    SPACE               TO  ORCSCHKTCHK-NYUHKTCD
                   MOVE    SPACE               TO  ORCSCHKTCHK-NYUSRYCD
                   PERFORM VARYING IDX-N       FROM    1   BY  1
                           UNTIL  (IDX-N       >   31  )
                       IF      ORCSCHKTCHK-NYUHKTFLG (IDX-N)   =  1
                           IF     (TBL-DEKIDAKA  (IDX-N)   =   ZERO )
                               MOVE    ZERO    TO  ORCSCHKTCHK-NYUHKTFLG
                                                            (IDX-N)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
      *R03.4   ¿ÇÎÅÆü¤¬Êñ³ç¤Ç½ÐÍè¹â»»Äê¤ÎÆü¤¬¤¢¤ì¤ÐÀßÄê
               IF     (TBL-DEKIDAKA  (IDX-D)   =   0  )
                   PERFORM VARYING IDX-N       FROM    1   BY  1
                           UNTIL  (IDX-N       >   31  )
                       IF      ORCSCHKTCHK-NYUHKTFLG (IDX-N)   =  1
                           IF     (TBL-DEKIDAKA  (IDX-N)   =   1  )
                               MOVE    ZERO    TO  ORCSCHKTCHK-NYUHKTFLG
                                                            (IDX-N)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *!!!
           ELSE
      *        Êñ³ç»»ÄêÆü¤«¤é½ÐÍè¹â»»ÄêÆü¤Ø
               PERFORM VARYING IDX-N       FROM    1   BY  1
                       UNTIL  (IDX-N       >   31  )
                   IF      ORCSCHKTCHK-NYUHKTFLG (IDX-N)   =  ZERO
                       IF     (TBL-DEKIDAKA  (IDX-N)   =   1  )
                           MOVE    1       TO  ORCSCHKTCHK-NYUHKTFLG
                                                          (IDX-N)
                       END-IF
                    END-IF
               END-PERFORM
           END-IF
      *
           MOVE    ZERO                TO  WRK-ERRDAY
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
                          OR  (SPA-ERRCD   NOT =   SPACE    )
               IF     (SPA-COM-HKNCOMBI (IDX-Z)    =   "9999")
                   OR (SPA-COM-SRYKBN   (IDX-Z)    =   "95"
                                                   OR  "96"
                                                   OR  "99"
                                                   OR  "98"  )
      *            ¼£¸³ÂÐ¾Ý³°
                   OR (SPA-COM-CHIKENKBN(IDX-Z)    =   "1"   )
      *            ÌôÉ¾ÂÐ¾Ý³°
                   OR  (SPA-COM-YAKUHYO (IDX-Z)    =   "1"   )
                   OR  (SPA-COM-GENTEN  (IDX-Z)    =   "1"   )
      *            ¥¢¥Õ¥¿¡¼¥±¥¢
                   OR  (SPA-RSI-HKNKBN             =   "3" )
                   CONTINUE
               ELSE
                   IF      SPA-GMN-INPUTCD (IDX-Z)(1:1)  =   "*"
                       PERFORM VARYING IDX-N       FROM    1   BY  1
                               UNTIL  (IDX-N       >   31  )
                                   OR (SPA-ERRCD   NOT =   SPACE )
                          IF      SPA-COM-NYUINDAY(IDX-Z IDX-N)
                                                   >   ZERO
                               IF      ORCSCHKTCHK-NYUHKTFLG (IDX-N)
                                                       =   ZERO
                                   MOVE    IDX-N       TO  WRK-ERRDAY
                                   MOVE    "0218"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                       END-PERFORM
                       IF      SPA-ERRCD       NOT =   SPACE
                           MOVE    "1"         TO  SPA-COM-CUR (IDX-Z)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  AND
                  (WRK-ERRDAY          >   ZERO  )
               MOVE    SPACE               TO  SPA-ERRMSG2
               STRING  WRK-ERRDAY
                       "Æü"                DELIMITED  BY  SIZE
                       INTO                SPA-ERRMSG2
               END-STRING
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        Êñ³ç¶èÊ¬¥¯¥ê¥¢
               PERFORM 602-HOUKATU-INIT-SEC
               IF      ORCSCHKTCHK-NYUHKTCD    NOT =   SPACE
      *            Æþ±¡ÎÁÊñ³ç¥Á¥§¥Ã¥¯
                   PERFORM 600-NYUHKT-SYORI-SEC
               END-IF
           END-IF
           .
       600-NYUINDAYCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç¶èÊ¬¥¯¥ê¥¢½èÍý
      *****************************************************************
       602-HOUKATU-INIT-SEC         SECTION.
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  ) 
               MOVE    ZERO            TO  SPA-COM-HOUKATU-KFLG(IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-FLG (IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-FLG2(IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-ZAI (IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-ERR (IDX-Z)
           END-PERFORM
           .
       602-HOUKATU-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁÊñ³ç¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       600-NYUHKT-SYORI-SEC         SECTION.
      *
           MOVE    1                   TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   TBL-N-MAX )
              IF       TBL-N-KANRICD (IDY) =   ORCSCHKTCHK-NYUHKTCD
                   MOVE    IDY                 TO  IDX
                   MOVE    TBL-N-MAX           TO  IDY
               END-IF
           END-PERFORM
           MOVE    1                       TO  ORCSCHKTCHK-HKTFLG
           MOVE    TBL-N-KANRICD (IDX)     TO  WRK-KANRICD
           MOVE    TBL-N-SANTEI-HOU (IDX)  TO  WRK-SANTEI-HOU
           MOVE    TBL-N-SANTEI-KBN (IDX)  TO  WRK-SANTEI-KBN
           MOVE    TBL-N-NYUGAIKBN (IDX)   TO  WRK-NYUGAIKBN
           MOVE    TBL-N-HKNCOMBIKBN (IDX) TO  WRK-HKNCOMBIKBN
           MOVE    TBL-N-SRYKBN-G    (IDX) TO  WRK-SRYKBN-G
      *    Êñ³çÂÐ¾ÝÆþ±¡ÎÁ¥³¡¼¥É
           MOVE    ORCSCHKTCHK-NYUSRYCD    TO  WRK-K-SRYCD
      *    ÆþÎÏ¹àÌÜ¤ÎÊñ³ç¥Á¥§¥¯
           PERFORM 2002-HKTSANTEI-CHK-SEC
      *    ºÞ¤ÎÊÔ½¸
           PERFORM 2002-ZAIHENSYU-SEC
           .
       600-NYUHKT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍýÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥ÉÆÉ¤ß¹þ¤ß
      *****************************************************************
       910-HKTSRYCD-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKTSRYCD-REC
               MOVE    ZERO            TO  FLG-HKTSRYCD
           ELSE
               INITIALIZE                  HKTSRYCD-REC
               MOVE    1               TO  FLG-HKTSRYCD
           END-IF
      *
           .
       910-HKTSRYCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥ÉÆÉ¤ß¹þ¤ß
      *****************************************************************
       910-HKTSANTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKTSANTEI-REC
               MOVE    ZERO            TO  FLG-HKTSANTEI
           ELSE
               INITIALIZE                  HKTSANTEI-REC
               MOVE    1               TO  FLG-HKTSANTEI
           END-IF
      *
           .
       910-HKTSANTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    »»ÄêÍúÎòÆÉ¤ß¹þ¤ß
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SANTEI-REC
               MOVE    ZERO            TO  FLG-SANTEI
           ELSE
               INITIALIZE                  SANTEI-REC
               MOVE    1               TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼¼¡ÆÉ¹þ
      *****************************************************************
       900-NYUINACCT-READ-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
               MOVE    ZERO                TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                      NYUINACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF.
      *
       900-NYUINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       930-NYUINACT-RAED-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF.
      *
       930-NYUINACT-RAED-EXT.
           EXIT.
      *****************************************************************
      *    ¼õ¿ÇÍúÎò¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥¹½èÍý
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
