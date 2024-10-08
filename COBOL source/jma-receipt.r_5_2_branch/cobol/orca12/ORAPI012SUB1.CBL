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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORAPI012SUB1.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ͽ
      *  ����ݡ��ͥ��̾  : �ݸ������ʬ̾�μ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  17/07/XX    NACL-¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 05.00.00     NACL-¿��Ǽ  20/11/XX  �����ʳ�ǧ�б��ǳ�礫�������ʬ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    ��������ΰ�
      *01  CNT-AREA.
      *
      *    ź���ΰ�
      *01  IDX-AREA.
      *    03  IDX                 PIC 9(04).
      *
      *    ����ΰ�
      *01  WRK-AREA.
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *
      *    �����Ѵ��ΰ�
      *    COPY    "CPORCSNUM.INC".
      *
      *   �����Ѵ�����
      *    COPY    "CPORCSDAY.INC".
      *    COPY    "CPORCSLNK.INC".
      *    �ģ¸���
      *    COPY    "MCPDATA.INC".
      *
      *
      *    COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �ѥ�᥿
           COPY    "CPORAPI012SUB1.INC".
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORAPI012SUB1AREA
           SPA-AREA.
      *
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      ***  INITIALIZE                  API012SUB1-OUT-AREA
      **   INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *
      *R02.11
      *    �������ô��礫�������ʬ�ֵѡʥ����ʡ�
           IF     (API012SUB1-HOJOKBN  =   SPACE )
             AND  (API012SUB1-RATE NOT =   ZERO  )
               PERFORM 1002-HOJOKBN-SET-SEC
           ELSE
      *
      *    �����ʬ̾�μ���
           PERFORM 100-HOJOKBN-HEN-SEC
      *
           END-IF
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    �����ʬ̾�μ�������
      *****************************************************************
       100-HOJOKBN-HEN-SEC               SECTION.
      *
      *
           EVALUATE    API012SUB1-HKNNUM
      *    ����
               WHEN    "002"
               EVALUATE    API012SUB1-HOJOKBN
                   WHEN    "1"
                       MOVE    "��̳" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    "2"
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    "3"
                       MOVE    "�̶�" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    "7"
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    "8"
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "9"
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "A"
                       MOVE    "���俦̳" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "B"
                       MOVE    "���䲼��" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "C"
                       MOVE    "�����̶�" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "D"
                       MOVE    "���俦̳" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "E"
                       MOVE    "���䲼��" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "F"
                       MOVE    "�����̶�" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "G"
                       MOVE    "���俦̳" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    "H"
                       MOVE    "���䲼��" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    "I"
                       MOVE    "�����̶�" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    SPACE
                       CONTINUE
                   WHEN    OTHER
                       MOVE    1           TO  API012SUB1-ERRCD
               END-EVALUATE
      *    ��������
               WHEN    "039"
               WHEN    "040"
               EVALUATE    API012SUB1-HOJOKBN
                   WHEN    "1"
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "2"
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "3"
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    OTHER
                       MOVE    1           TO  API012SUB1-ERRCD
               END-EVALUATE
      *        ����
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
               EVALUATE    API012SUB1-HOJOKBN
                   WHEN    "2"
                       MOVE    "��������"
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    "7"
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    "8"
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "9"
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "B"
                       MOVE    "���䲼��" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "E"
                       MOVE    "���䲼��" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "H"
                       MOVE    "���䲼��" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    SPACE
                       CONTINUE
                   WHEN    OTHER
                       MOVE    1           TO  API012SUB1-ERRCD
               END-EVALUATE
      *        ����
               WHEN    "060"
               WHEN    "068"
               EVALUATE    API012SUB1-HOJOKBN
                   WHEN    "0"
                       MOVE    "̵��"
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    00          TO  API012SUB1-RATE
                   WHEN    "1"
                       MOVE    "����"
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "2"
                       MOVE    "����"
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "3"
                       MOVE    "����"
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    "4"
                       MOVE    "������ ������"
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "5"
                       MOVE    "������ ������"
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "6"
                       MOVE    "������ ������"
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    SPACE
                       CONTINUE
                   WHEN    OTHER
                       MOVE    1           TO  API012SUB1-ERRCD
               END-EVALUATE
      *        ����
               WHEN    "900"
               WHEN    "901"
               EVALUATE    API012SUB1-HOJOKBN
                   WHEN    "1"
                       MOVE    "����"
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    "2"
                       MOVE    "�����"
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    OTHER
                       MOVE    1           TO  API012SUB1-ERRCD
               END-EVALUATE
      *        ����
               WHEN    "980"   THRU   "989"
               EVALUATE    API012SUB1-HOJOKBN
                   WHEN    "1"
                       MOVE    "����"
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    "2"
                       MOVE    "�����"
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    OTHER
                       MOVE    1           TO  API012SUB1-ERRCD
               END-EVALUATE
      *
               WHEN    OTHER
                   IF      API012SUB1-HKNNUM(1:1)  =   "9"
                       CONTINUE
                   ELSE
      *            ������
                   EVALUATE    API012SUB1-HOJOKBN
                       WHEN    "9"
                           MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                           MOVE    10      TO  API012SUB1-RATE
                       WHEN    "8"
                           MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                           MOVE    20      TO  API012SUB1-RATE
                       WHEN    "7"
                           MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                           MOVE    30      TO  API012SUB1-RATE
                       WHEN    SPACE
                           CONTINUE
                       WHEN    OTHER
                           MOVE    1           TO  API012SUB1-ERRCD
                   END-EVALUATE
                   END-IF
           END-EVALUATE
      *
           .
       100-HOJOKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ʬ̾�μ�������
      *****************************************************************
       1002-HOJOKBN-SET-SEC               SECTION.
      *
      *
           EVALUATE    API012SUB1-HKNNUM
      *    ����
               WHEN    "002"
               EVALUATE    API012SUB1-RATE
                   WHEN    30
                       MOVE    "7"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    20
                       MOVE    "8"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    10
                       MOVE    "9"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
               END-EVALUATE
      *    ��������
               WHEN    "039"
               WHEN    "040"
               EVALUATE    API012SUB1-RATE
                   WHEN    10
                       MOVE    "1"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    20
                       MOVE    "2"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    30
                       MOVE    "3"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
               END-EVALUATE
      *        ����
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
               EVALUATE    API012SUB1-RATE
                   WHEN    30
                       MOVE    "7"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    20
                       MOVE    "8"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    10
                       MOVE    "9"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
               END-EVALUATE
      *        ����
               WHEN    "060"
               WHEN    "068"
               EVALUATE    API012SUB1-RATE
                   WHEN    10
                       MOVE    "1"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    20
                       MOVE    "2"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    30
                       MOVE    "3"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
               END-EVALUATE
      *
               WHEN    OTHER
               IF      API012SUB1-HKNNUM(1:1)  =   "9"
                   CONTINUE
               ELSE
      *        ������
               EVALUATE    API012SUB1-RATE
                   WHEN    30
                       MOVE    "7"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    20
                       MOVE    "8"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    10
                       MOVE    "9"         TO  API012SUB1-HOJOKBN
                       MOVE    "����" 
                                           TO  API012SUB1-HOJOKBN-NAME
               END-EVALUATE
               END-IF
           END-EVALUATE
      *
           .
       100-HAKKOU-EXT.
           EXIT.
      *
      *
      *
