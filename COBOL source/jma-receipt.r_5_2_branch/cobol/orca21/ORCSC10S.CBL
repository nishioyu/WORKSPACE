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
       PROGRAM-ID.                 ORCSC10S.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ���Ź԰ٿǻ�����ưȯ���Խ�����
      *                      �ʣˣ����Τ߶��̻��ѡ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/03/01    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  03/05/23  �ȣ���ǯ��������ɲ�
      * 01.00.02     NACL-¿��Ǽ  04/03/16  �ȣ���ǯ��������ɲ�
      * 01.00.03     NACL-¿��Ǽ  06/03/16  �ȣ���ǯ��������ɲ�
      * 04.05.00     NACL-¿��Ǽ  10/03/XX  �ȣ���ǯ��������ɲ�
      * 04.07.00     NACL-¿��Ǽ  13/03/XX  �ȣ���ǯ����ϫ�ҥ쥻���б�
      * 04.08.00     NACL-¿��Ǽ  18/03/XX  �ȣ���ǯ��������ɲ�
      * 05.00.00     NACL-¿��Ǽ  20/03/XX  �ң���ǯ��������ɲ�
      * 05.01.00     ORCAMMO      22/03/XX  �ң���ǯ��������ɲ�
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
           COPY    "CPORCSC10S.INC".
      *
      *    �ӣУ��ѥ�᥿
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    ���̥����ΰ�
           COPY    "K02SCR-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSC10SAREA
           SPA-AREA
           SPA-SCR-REC
           SPA-K02
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           EVALUATE    TRUE
               WHEN    SPA-SRYYMD          <=  "20020331"
                   CALL    "ORCSC10S200203"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20020401" )  AND
                      (SPA-SRYYMD          <   "20030601" )
                   CALL    "ORCSC10S200204"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20030601" )  AND
                      (SPA-SRYYMD          <   "20040401" )
                   CALL    "ORCSC10S200306"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20040401" )  AND
                      (SPA-SRYYMD          <   "20060401" )
                   CALL    "ORCSC10S200404"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20060401" )  AND
                      (SPA-SRYYMD          <   "20080401" )
                   CALL    "ORCSC10S200604"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20080401" )  AND
                      (SPA-SRYYMD          <   "20100401" )
                   CALL    "ORCSC10S200804"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20100401" )  AND
                      (SPA-SRYYMD          <   "20120401" )
                   CALL    "ORCSC10S201004"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20120401" )  AND
      *!!             (SPA-SRYYMD          <   "99999999" )
                      (SPA-SRYYMD          <   "20130701" )
                   CALL    "ORCSC10S201204"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20130701" )  AND
                      (SPA-SRYYMD          <   "20140401" )
                   CALL    "ORCSC10S201307"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20140401" )  AND
                      (SPA-SRYYMD          <   "20160401" )
                   CALL    "ORCSC10S201404"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20160401" )  AND
                      (SPA-SRYYMD          <   "20180401" )
                   CALL    "ORCSC10S201604"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20180401" )  AND
                      (SPA-SRYYMD          <   "20200401" )
                   CALL    "ORCSC10S201804"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20200401" )  AND
                      (SPA-SRYYMD          <   "20220401" )
                   CALL    "ORCSC10S202004"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20220401" )  AND
                      (SPA-SRYYMD          <   "20240601" )
                   CALL    "ORCSC10S202204"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20240601" )  AND
                      (SPA-SRYYMD          <   "99999999" )
                   CALL    "ORCSC10S202406"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
           END-EVALUATE
      *
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
