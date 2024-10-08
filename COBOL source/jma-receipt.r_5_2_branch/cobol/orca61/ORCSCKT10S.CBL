      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSCKT10S.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����ޤȤ�
      *  ����ݡ��ͥ��̾  : ���Ź԰ٿǻ�����ưȯ���Խ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  09/02/27    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 04.05.00     NACL-¿��Ǽ  10/03/XX  �ȣ���ǯ��������б�
      * 04.07.00     NACL-¿��Ǽ  12/03/XX  �ȣ���ǯ��������б�
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
      *WORKING-STORAGE             SECTION.
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    �ѥ�᥿
           COPY    "CPORCSCKT10S.INC".
      *
      *    �ӣУ��ѥ�᥿
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    ���̥����ΰ�
           COPY    "KT01SCR-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSCKT10SAREA
           SPA-AREA
           SPA-SCR-REC
           SPA-KT01
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           EVALUATE    TRUE
               WHEN   (SPA-SRYYMD          >=  "20080401" )  AND
                      (SPA-SRYYMD          <   "20100401" )
                   CALL    "ORCSCKT10S200804"  USING
                                               ORCSCKT10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-KT01
      *
               WHEN   (SPA-SRYYMD          >=  "20100401" )  AND
                      (SPA-SRYYMD          <   "20120401" )
                   CALL    "ORCSCKT10S201004"  USING
                                               ORCSCKT10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-KT01
      *
               WHEN   (SPA-SRYYMD          >=  "20120401" )  AND
                      (SPA-SRYYMD          <   "20140401" )
                   CALL    "ORCSCKT10S201204"  USING
                                               ORCSCKT10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-KT01
      *
               WHEN   (SPA-SRYYMD          >=  "20140401" )  AND
                      (SPA-SRYYMD          <   "20160401" )
                   CALL    "ORCSCKT10S201404"  USING
                                               ORCSCKT10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-KT01
      *
               WHEN   (SPA-SRYYMD          >=  "20160401" )  AND
                      (SPA-SRYYMD          <   "20180401" )
                   CALL    "ORCSCKT10S201604"  USING
                                               ORCSCKT10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-KT01
      *
               WHEN   (SPA-SRYYMD          >=  "20180401" )  AND
                      (SPA-SRYYMD          <   "20200401" )
                   CALL    "ORCSCKT10S201804"  USING
                                               ORCSCKT10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-KT01
      *
               WHEN   (SPA-SRYYMD          >=  "20200401" )  AND
                      (SPA-SRYYMD          <   "20220401" )
                   CALL    "ORCSCKT10S202004"  USING
                                               ORCSCKT10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-KT01
      *
               WHEN   (SPA-SRYYMD          >=  "20220401" )  AND
                      (SPA-SRYYMD          <   "20240601" )
                   CALL    "ORCSCKT10S202204"  USING
                                               ORCSCKT10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-KT01
               WHEN   (SPA-SRYYMD          >=  "20240601" )  AND
                      (SPA-SRYYMD          <   "99999999" )
                   CALL    "ORCSCKT10S202406"  USING
                                               ORCSCKT10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-KT01
           END-EVALUATE
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
