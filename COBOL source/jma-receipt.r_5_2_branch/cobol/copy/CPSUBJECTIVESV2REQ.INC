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
      *    ���Уɾɾ��ܵ���Ͽ������� (xml2)
      *                                   COPY (CPSUBJECTIVEV2REQ.INC)
      ******************************************************************
        01  XML-SUBJECTIVESREQ.
            03  SUBJETREQ-CONTEXT           PIC S9(9)   BINARY.
            03  SUBJETREQ-OBJECT            PIC X(36).
            03  SUBJETREQ-MODE              PIC S9(9)   BINARY.
            03  SUBJETREQ-RECORDNAME        PIC X(128).
            03  SUBJETREQ-MEDICALREQ.
      *       ������ʬ
              05  SUBJETREQ-OUTPATIENT-CLASS  PIC X(1).
      *       �����ֹ�
              05  SUBJETREQ-PATIENTID         PIC X(20).
      *       ����ǯ��
              05  SUBJETREQ-PERFORM-DATE      PIC X(10).
      *       ���Ų�
              05  SUBJETREQ-DEP-CODE          PIC X(02).
      *       �ݸ��ȹ礻�ֹ�
              05  SUBJETREQ-COMBINATION-NUMBER     PIC X(04).
      *       �ݸ��ȹ礻����
              05  SUBJETREQ-INSURANCE-INFO.
      *           �ݸ��μ���
                  07  SUBJETREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *           �ݸ��μ���
                  07  SUBJETREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *           �ݸ����ֹ�
                  07  SUBJETREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *           ����
                  07  SUBJETREQ-MAIN-MARK               PIC X(80).
      *           �ֹ�
                  07  SUBJETREQ-MAIN-NUMBER             PIC X(80).
      *R03.01  ����
                  07  SUBJETREQ-MAIN-EDABAN             PIC X(02).
      *           ��³��ʬ
                  07  SUBJETREQ-MAIN-CONTINUATION      PIC X(01).
      *           �����ʬ
                  07  SUBJETREQ-MAIN-ASSISTANCE        PIC X(01).
      *           �ܿͲ�²��ʬ
                  07  SUBJETREQ-MAIN-FAMILY            PIC X(01).
      *           ���ݸ���̾
                  07  SUBJETREQ-MAIN-POLICYHOLDER      PIC X(100).
      *           ͭ��������
                  07  SUBJETREQ-MAIN-START-DATE        PIC X(10).
      *           ͭ����λ��
                  07  SUBJETREQ-MAIN-END-DATE          PIC X(10).
      *           �������
                  07  SUBJETREQ-PUBLIC-INSURANCE-INF  OCCURS  4 TIMES.
      *             ����μ���
                    09  SUBJETREQ-INSURANCE-CLASS       PIC X(03).
      *             ����μ���
                    09  SUBJETREQ-INSURANCE-NAME        PIC X(20).
      *             ��ô���ֹ�
                    09  SUBJETREQ-PROVIDER-NUMBER       PIC X(08).
      *             ������ֹ�
                    09  SUBJETREQ-RECIPIENT-NUMBER      PIC X(20).
      *             ͭ��������
                    09  SUBJETREQ-INSURANCE-START-DATE  PIC X(10).
      *             ͭ����λ��
                    09  SUBJETREQ-INSURANCE-END-DATE    PIC X(10).
      *       �ɾ��ܵ���ʬ������
              05  SUBJETREQ-SUBJECTIVES-CODE       PIC X(02).
      *       �ɾ��ܵ�
              05  SUBJETREQ-DETAIL-RECORD          PIC X(4800).
      *
