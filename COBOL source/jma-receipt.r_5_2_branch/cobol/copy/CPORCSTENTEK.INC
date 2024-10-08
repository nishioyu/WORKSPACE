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
      ************************************************************
      *    ������ũ���ץѥ�᥿����COPY (CPORCSCTENTEK.INC)
      *                            ���ѥץ������ 
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  03/04/03  �濴��̮�����ɲ�
      * 04.07.00     NACL-¿��Ǽ  11/11/XX  Ʊ���������ɲ�
      * 05.02.00     ORCAMO       23/06/XX  �����ȣ������
      ************************************************************
       01  ORCSTENTEKAREA.
      *
      *����
           03  ORCSTEN-IN-AREA.
      *������ʬ�ʣ�����繹���������ݸ��̡������������å���
               05  ORCSTEN-KBN                 PIC  X(01).
      *���ԣɣ�
               05  ORCSTEN-PTID                PIC  9(10).
      *����ǯ��
               05  ORCSTEN-SRYYM               PIC  X(06).
      *�ݸ��ȹ�
               05  ORCSTEN-HKNCOMBI            PIC 9(04).
      *���ϡ��ݸ��̡�
           03  ORCSTEN-OUT-AREA.
      *        �������̥ơ��֥�
               05  ORCSTEN-YAKZI-TBL           OCCURS   31.
      *            ������
                   07  ORCSTEN-ZAITEN          PIC  9(08).
      *            ���Ų�
                   07  ORCSTEN-YK-SRYKA        PIC  X(02).
      *            ��ױ���
                   07  ORCSTEN-CHUSYARYO       PIC  9(07)V9(05).
      *
                   07  ORCSTEN-MEISAI-OCC      OCCURS   50.
      *                ���ť�����
                       09  ORCSTEN-SRYCD       PIC X(09).
      *                ����
                       09  ORCSTEN-SURYO       PIC 9(05)V9(05).
      *                �����Ѵ�������
                       09  ORCSTEN-HAIK-SRYCD  PIC X(09).
      *                �Ѵ�����
                       09  ORCSTEN-HAIK-SURYO   PIC 9(05)V9(05).
      *                ������
      ****             09  ORCSTEN-COMMNT-G        OCCURS  5.
                       09  ORCSTEN-COMMNT-G        OCCURS  20.
                           11  ORCSTEN-COM-SRYCD       PIC X(09).
      *                    ���ֹ�
                           11  ORCSTEN-COM-ZAINUM      PIC  9(08).
      *                    �����Ȼ����ֹ�
                           11  ORCSTEN-COM-RENNUM      PIC  9(03).
                           11  ORCSTEN-COM-MEI         PIC  X(140).
      *                    �������ΰ�
                           11  ORCSTEN-COM-INPUTCHI-AREA.
                               13  ORCSTEN-COM-INPUTCHI   PIC  X(10)
                                                          OCCURS  5.
      *                    ���ϥ���������
                           11  ORCSTEN-COM-INPUTSTRING   PIC  X(200).
      *
      *        �����̥ơ��֥�
               05  ORCSTEN-KIZAI-TBL-G.
                   07  ORCSTEN-KIZI-ZAI-OCC        OCCURS   50.
      *                �������
                       09  ORCSTEN-KIZ-ZAITEN          PIC  9(08).
                       09  ORCSTEN-KIZ-MEI-OCC         OCCURS   10.
      *                    �����ť�����
                           11  ORCSTEN-KIZ-SRYCD       PIC X(09).
      *                    ������
                           11  ORCSTEN-KIZ-SURYO       PIC 9(05)V9(05).
      *                    ������
                           11  ORCSTEN-KIZ-COMMNT-G        OCCURS  20.
                               13  ORCSTEN-KIZ-COM-SRYCD   PIC X(09).
      *                        ���ֹ�
                               13  ORCSTEN-KIZ-ZAINUM      PIC  9(08).
      *                        �����Ȼ����ֹ�
                               13  ORCSTEN-KIZ-RENNUM      PIC  9(03).
                               13  ORCSTEN-KIZ-COM-MEI     PIC X(140).
      *                        �������ΰ�
                               13  ORCSTEN-KIZ-INPUTCHI-AREA.
                                   15  ORCSTEN-KIZ-INPUTCHI
                                                           PIC  X(10)
                                                           OCCURS  5.
      *                        ���ϥ���������
                               13  ORCSTEN-KIZ-INPUTSTRING
                                                           PIC  X(200).
      *                �������
                       09  ORCSTEN-KIZ-DAY-G.
                           11  ORCSTEN-KIZ-DAY         PIC 9(03)
                                                       OCCURS  31.
      *                    Ʊ��������
                           11  ORCSTEN-KIZ-DAY2-G      OCCURS  31.
                               13  ORCSTEN-KIZ-DAY2    PIC 9(03)
                                                       OCCURS  2.
      *                    ���Ų�
                           11  ORCSTEN-KIZ-SRYKA       PIC X(02)
                                                       OCCURS  31.
      *
      *��ũ�굻������
           03  ORCSTEN-DAY-AREA.
      *        ̤������
               05  ORCSTEN-MIDAY-AREA.
                   07  ORCSTEN-MI-DAY          PIC 9(01)
                                               OCCURS   31.
               05  ORCSTEN-MIIVHDAY-AREA.
      *            �濴��̮
                   07  ORCSTEN-MIIVH-DAY       PIC 9(01)
                                               OCCURS   31.
      *        ʣ��������
               05  ORCSTEN-FUKUDAY-AREA.
      *            ��ũ
                   07  ORCSTEN-FUKU-AREA.
                       09  ORCSTEN-FUKU-DAY        PIC 9(01)
                                               OCCURS   31.
      *            ư̮
                   07  ORCSTEN-DOU-AREA.
                       09  ORCSTEN-DOU-DAY         PIC 9(01)
                                               OCCURS   31.
      *            �濴��̮
                   07  ORCSTEN-CHU-AREA.
                       09  ORCSTEN-CHU-DAY         PIC 9(01)
                                               OCCURS   31.
      *Ʊ���������б�����
           03  ORCSTEN-DOUJITU-AREA.
      *        �������̥ơ��֥�
               05  ORCSTEN-DOU-TBL2          OCCURS   31.
      *            ���ޣ�������
                   07  ORCSTEN-DOU-ZAITEN          PIC  9(08)
                                                   OCCURS   2.
      *
      *
      *���顼�����ɡʣ�����׾Ȳ񹹿����顼
      *              �����������򹹿����顼��
           03  ORCSTEN-ERRCD           PIC 9(01).
      *�����ե饰�ʣ����ѹ������
           03  ORCSTEN-HENFLG          PIC 9(01).
      *�����ե饰���ʣ���������顼��
           03  ORCSTEN-HENFLG2         PIC 9(01).
      *
