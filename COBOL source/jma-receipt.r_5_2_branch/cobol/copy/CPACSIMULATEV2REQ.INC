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
      *    ���Уɥꥯ��������ô��׻�����
      *                                    COPY (CPACSIMULATEV2REQ.INC)
      ******************************************************************
        01  XML-ACSIMULATEREQ.
            02  ACSREQ-CONTEXT          PIC S9(9)   BINARY.
            02  ACSREQ-OBJECT           PIC X(36).
            02  ACSREQ-MODE             PIC S9(9)   BINARY.
            02  ACSREQ-RECORDNAME       PIC X(128).
            02  ACSREQ-MEDICALREQ.
      *       �����ֹ�
              03  ACSREQ-PATIENTID         PIC X(20).
      *       ��������
              03  ACSREQ-PERFORM-DATE      PIC X(10).
      *       ���ֳ���ʬ�ʿǻ�����
              03  ACSREQ-TIME-CLASS        PIC X(1).
      *
              03  ACSREQ-MEDICAL-INFO.
      *         ���Ų�
                04  ACSREQ-APPOINT-DEP-CODE    PIC X(2).
      *         �ݸ��ȹ礻�ֹ�
                04  ACSREQ-COMBINATION-NUMBER  PIC X(4).
      *         �ݸ�����
                04  ACSREQ-INSURANCE-INFO.
      *           �ݸ��μ���
                  05  ACSREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *           �ݸ����ֹ�
                  05  ACSREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *           �ݸ��μ���̾��
                  05  ACSREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *           ����
                  05  ACSREQ-MAIN-MARK               PIC X(80).
      *           �ֹ�
                  05  ACSREQ-MAIN-NUMBER             PIC X(80).
      *R03.1      ����
                  05  ACSREQ-MAIN-EDABAN             PIC X(02).
      *           ��³��ʬ
                  05  ACSREQ-MAIN-CONTINUATION       PIC X(01).
      *           �����ʬ
                  05  ACSREQ-MAIN-ASSISTANCE         PIC X(01).
      *           �ܿͲ�²��ʬ
                  05  ACSREQ-MAIN-FAMILY             PIC X(01).
      *           ���ݸ���̾
                  05  ACSREQ-MAIN-POLICYHOLDER       PIC X(100).
      *           ͭ��������
                  05  ACSREQ-MAIN-START-DATE         PIC X(10).
      *           ͭ����λ��
                  05  ACSREQ-MAIN-END-DATE           PIC X(10).
      *           �������
                  05  ACSREQ-PUBLIC-INSURANCE-INF  OCCURS  4 TIMES.
      *             ����μ���
                    06  ACSREQ-INSURANCE-CLASS       PIC X(03).
      *             ����μ���̾��
                    06  ACSREQ-INSURANCE-NAME        PIC X(20).
      *             ��ô���ֹ�
                    06  ACSREQ-PROVIDER-NUMBER       PIC X(08).
      *             ������ֹ�
                    06  ACSREQ-RECIPIENT-NUMBER      PIC X(20).
      *             ͭ��������
                    06  ACSREQ-INSURANCE-START-DATE  PIC X(10).
      *             ͭ����λ��
                    06  ACSREQ-INSURANCE-END-DATE    PIC X(10).
      *         ��������
                04  ACSREQ-MDC-INFO         OCCURS  40 TIMES.
      *           ���ż��̶�ʬ
                  05  ACSREQ-MDC-CLASS               PIC X(3).
      *??
                  05  ACSREQ-MDC-CLASS-NAME          PIC X(50).
      *           ���
                  05  ACSREQ-MDC-CLASS-NUMBER        PIC X(50).
      *           ������
                  05  ACSREQ-PRESCRIPTION-INFO     OCCURS  40 TIMES.
      *             ���ť�����
                    06  ACSREQ-PRSCRPT-CODE          PIC X(9).
      *             ����̾�Ρʥ����ȡ�
                    06  ACSREQ-PRSCRPT-NAME          PIC X(80).
      *             ����
                    06  ACSREQ-PRSCRPT-NUMBER        PIC X(40).
      *             �����������̾����
                    06  ACSREQ-GENERIC-FLG           PIC X(03).
