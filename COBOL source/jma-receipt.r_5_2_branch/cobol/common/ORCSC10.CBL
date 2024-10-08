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
       PROGRAM-ID.                 ORCSC10.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ���Ź԰٥����å�������ʿ��š�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     MCC��¿��Ǽ  01/04/19  ���ֳ��û������Ļ��û��ν�����
      * 02.00.00     MCC��¿��Ǽ  01/08/09  ���Τν���
      * 02.00.01     MCC��¿��Ǽ  02/03/01  �������ޡ�������β����
      *                                     ��Τߤ�
      * 02.00.02     MCC��¿��Ǽ  02/03/11  ������˥��֥ץ���ʬΥ
      * 02.00.03     NACL-¿��Ǽ  03/05/23  �ȣ���ǯ��������ɲ�
      * 02.00.04     NACL-¿��Ǽ  04/03/00  �ȣ���ǯ��������ɲ�
      * 02.00.05     NACL-¿��Ǽ  06/03/XX  �ȣ���ǯ��������ɲ�
      * 04.02.00     NACL-¿��Ǽ  08/03/XX  �ȣ���ǯ��������ɲ�
      * 04.05.00     NACL-¿��Ǽ  10/03/XX  �ȣ���ǯ��������ɲ�
      * 04.07.00     NACL-¿��Ǽ  13/03/XX  �ȣ���ǯ����ϫ�ҥ쥻���б�
      * 04.07.00     NACL-¿��Ǽ  14/03/XX  �ȣ���ǯ��������ɲ�
      * 04.08.00     NACL-¿��Ǽ  16/03/XX  �ȣ���ǯ��������ɲ�
      * 04.08.00     NACL-¿��Ǽ  18/03/XX  �ȣ���ǯ��������ɲ�
      * 05.00.00     NACL-¿��Ǽ  20/03/XX  �ң���ǯ��������ɲ�
      * 05.01.00     ORCAMO       22/03/XX  �ң���ǯ��������ɲ�
      *****************************************************************
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
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *    ���Ź԰ٻ����Ѷ��̥ѥ�᥿
           COPY    "CPORCSC00.INC".
      *
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC00AREA
           WRK-SCR-REC
           SPA-AREA
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
                   CALL    "ORCSC10200203"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
      *
               WHEN   (SPA-SRYYMD          >=  "20020401" )  AND
                      (SPA-SRYYMD          <   "20030601" )
                   CALL    "ORCSC10200204"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
      *
               WHEN   (SPA-SRYYMD          >=  "20030601" )  AND
                      (SPA-SRYYMD          <   "20040401" )
                   CALL    "ORCSC10200306"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
      *
               WHEN   (SPA-SRYYMD          >=  "20040401" )  AND
                      (SPA-SRYYMD          <   "20050401" )
                   CALL    "ORCSC10200404"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20050401" )  AND
                      (SPA-SRYYMD          <   "20060401" )
                   CALL    "ORCSC10200504"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20060401" )  AND
                      (SPA-SRYYMD          <   "20080401" )
                   CALL    "ORCSC10200604"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20080401" )  AND
                      (SPA-SRYYMD          <   "20100401" )
                   CALL    "ORCSC10200804"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20100401" )  AND
                      (SPA-SRYYMD          <   "20120401" )
                   CALL    "ORCSC10201004"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20120401" )  AND
                      (SPA-SRYYMD          <   "20130701" )
                   CALL    "ORCSC10201204"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20130701" )  AND
                      (SPA-SRYYMD          <   "20140401" )
                   CALL    "ORCSC10201307"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20140401" )  AND
                      (SPA-SRYYMD          <   "20160401" )
                   CALL    "ORCSC10201404"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20160401" )  AND
                      (SPA-SRYYMD          <   "20180401" )
                   CALL    "ORCSC10201604"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20180401" )  AND
                      (SPA-SRYYMD          <   "20200401" )
                   CALL    "ORCSC10201804"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20200401" )  AND
                      (SPA-SRYYMD          <   "20220401" )
                   CALL    "ORCSC10202004"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20220401" )  AND
                      (SPA-SRYYMD          <   "20240601" )
                   CALL    "ORCSC10202204"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20240601" )  AND
                      (SPA-SRYYMD          <   "99999999" )
                   CALL    "ORCSC10202406"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
           END-EVALUATE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
