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
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���У�̤�������� 
      *                             COPY (CPUNCLAIMEDV3REQ.INC)
      ******************************************************************
        01  XML-UNCLAIMEDREQ.
            02  UNCLAIMEDQ-CONTEXT          PIC S9(9)   BINARY.
            02  UNCLAIMEDQ-OBJECT           PIC X(36).
            02  UNCLAIMEDQ-MODE             PIC S9(9)   BINARY.
            02  UNCLAIMEDQ-RECORDNAME       PIC X(128).
            02  UNCLAIMEDQ-REQ.
      *         �ꥯ�������ֹ�
                03  UNCLAIMEDQ-REQUEST-NUMBER
                                           PIC X(2).
      *         �����UID
                03  UNCLAIMEDQ-KARTE-UID   PIC X(36).
      *         ���륫UID
                03  UNCLAIMEDQ-ORCA-UID    PIC X(36).
      *         ����ǯ��
                03  UNCLAIMEDQ-PERFORM-MONTH
                                           PIC X(7).
      *         ������ʬ
                03  UNCLAIMEDQ-INOUT       PIC X(1).
      *         ̤�������괵�԰���
                03  UNCLAIMEDQ-UNCLAIMED-INF.
                    05  UNCLAIMEDQ-UNCLAIMED-OCC       OCCURS  100.
      *                 �����ֹ�
                        07  UNCLAIMEDQ-UNCLAIMED-PTNUM PIC X(20).
      *         ̤�������������԰���
                03  UNCLAIMEDQ-CLAIMED-INF.
                    05  UNCLAIMEDQ-CLAIMED-OCC         OCCURS  100.
      *                 �����ֹ�
                        07  UNCLAIMEDQ-CLAIMED-PTNUM   PIC X(20).
      *                 ����ǯ��
                        07  UNCLAIMEDQ-CLAIMED-MONTH   PIC X(7).
      *
