      *****************************************************************
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
      *****************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *****************************************************************
      *    ���Уɻ�ʳ�ǧ��       COPY (CPONLINEQUAREQ1.INC)          *
      *****************************************************************
        01  XML-ONLINEQUAREQ1.
            02  ONLINEQUAREQ1-CONTEXT      PIC S9(9)   BINARY.
            02  ONLINEQUAREQ1-OBJECT       PIC X(36).
            02  ONLINEQUAREQ1-MODE         PIC S9(9)   BINARY.
            02  ONLINEQUAREQ1-RECORDNAME   PIC X(128).
            02  ONLINEQUAREQ1-DATAREQ.
      *         �����ֹ�
                03  ONLINEQUAREQ1-PATIENT-ID   PIC X(20).
      *         �»���
                03  ONLINEQUAREQ1-PERFORM-DATE PIC X(10).
      *         �»ܻ���
                03  ONLINEQUAREQ1-PERFORM-TIME PIC X(8).
      *         UUID
                03  ONLINEQUAREQ1-UUID         PIC X(36).
