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
      *    ���Ź԰پȲ�ơ��֥�      COPY (CPSRYSRH.INC)         *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SRYSRH-REC.
      *���ŵ����ֹ�
           03  SRYSRH-HOSPNUM      PIC 9(2).
      *ü���ɣ�
           03  SRYSRH-TERMID       PIC X(64).
      *���ԣɣ�
           03  SRYSRH-PTID         PIC 9(10).
      *���ֹ�
           03  SRYSRH-ZAINUM       PIC 9(09).
      *������ʬ
           03  SRYSRH-NYUGAIKBN    PIC X(1).
      *���Ų�
           03  SRYSRH-SRYKA        PIC X(2).
      *�ݸ��ȹ礻
           03  SRYSRH-HKNCOMBINUM  PIC 9(4).
      *���Ŷ�ʬ
           03  SRYSRH-SRYKBN       PIC X(2).
      *���ż��̶�ʬ
           03  SRYSRH-SRYSYUKBN    PIC X(3).
      *������
           03  SRYSRH-SRYYMD       PIC X(8).
      *�޲��
           03  SRYSRH-ZAIKAISU     PIC 9(5).
      *���̥ȡ�����
           03  SRYSRH-SURYOTOTAL   PIC 9(7)V9(05).
      *Ϣ��
           03  SRYSRH-RENNUM       PIC 9(02).
      *���ť�����
           03  SRYSRH-SRYCD-G.
               05  SRYSRH-SRYCD-OCC    OCCURS 5.
                   07  SRYSRH-SRYCD    PIC X(9).
                   07  SRYSRH-SRYSURYO PIC 9(05)V9(05).
      *����ʬ
           03  SRYSRH-HOKATSUKBN   PIC X(01).
      *��׶�ʬ��1:���Ų�׺� 2:������׺ޡ�
           03  SRYSRH-ACCTKBN      PIC X(01).
      *
      *������
           03  SRYSRH-CREYMD       PIC X(8).
      *������
           03  SRYSRH-UPYMD        PIC X(8).
      *��������
           03  SRYSRH-UPHMS        PIC X(6).
      *Ϣ�ֹ�
           03  SRYSRH-SEQNUM       PIC 9(9).
      *
      *    �ʲ�virtual����
      *�����ֹ�
           03  SRYSRH-PTNUM        PIC X(20).
      *���ʻ�̾
           03  SRYSRH-KANANAME     PIC X(100).
      *����
           03  SRYSRH-SEX          PIC X(1).
      *������
           03  SRYSRH-BIRTHDAY     PIC X(8).
      *���Ͽ�����
           03  SRYSRH-STSRYYMD     PIC X(8).
      *��λ������
           03  SRYSRH-EDSRYYMD     PIC X(8).
      *���Ͽ��ŷ�
           03  SRYSRH-STSRYYM      PIC X(6).
      *��λ���ŷ�
           03  SRYSRH-EDSRYYM      PIC X(6).
      *�ɥ�����
           03  SRYSRH-DRCD         PIC X(5).
      *SQL
           03  SRYSRH-SQL          PIC X(15000).
      *���աܻ���
           03  SRYSRH-YMDHMS       PIC X(14).

