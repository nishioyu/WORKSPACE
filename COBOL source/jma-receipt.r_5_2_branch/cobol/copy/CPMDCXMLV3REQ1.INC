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
      *    ���Уɥꥯ�����ȿ��Ź԰����Ͼ��� (���β�)  COPY (CPMDCXMLV3REQ.INC)
      ******************************************************************
       01  XML-MEDICALV3REQ1.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       �ꥯ�������ֹ�
              05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *       UID���Żҥ���ơ�
              05  MDCREQ-KARTE-UID         PIC X(36).
      *       ������աʼ�Ǽ��������
              05  MDCREQ-BASE-DATE         PIC X(10).
      *       �����ֹ�
              05  MDCREQ-PATIENTID         PIC X(20).
      *       ��������
              05  MDCREQ-PERFORM-DATE      PIC X(10).
      *       ���Ż���
              05  MDCREQ-PERFORM-TIME      PIC X(8).
      *       UID�ʥ��륫��
              05  MDCREQ-ORCA-UID          PIC X(36).
      *H30.XX
      *       ������ʬ
              05  MDCREQ-PATIENT-MODE      PIC X(06).
      *
              05  MDCREQ-DIAGNOSIS-INFO.
      *         ���Ų�
                07  MDCREQ-APPOINT-DEP-CODE    PIC X(2).
      *         �ɥ�����������
                07  MDCREQ-APPOINT-DR-CODE     PIC X(5).
      *         �ݸ�����
                07  MDCREQ-INSURANCE-INFO.
      *           �ݸ��ȹ礻�ֹ�
                  09  MDCREQ-COMBINATION-NUMBER      PIC X(04).
      *           �ݸ��μ���
                  09  MDCREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *           �ݸ����ֹ�
                  09  MDCREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *           �ݸ��μ���̾��
                  09  MDCREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *           ����
                  09  MDCREQ-MAIN-MARK               PIC X(80).
      *           �ֹ�
                  09  MDCREQ-MAIN-NUMBER             PIC X(80).
      *R03.01  ����
                  09  MDCREQ-MAIN-EDABAN             PIC X(02).
      *           ��³��ʬ
                  09  MDCREQ-MAIN-CONTINUATION       PIC X(01).
      *           �����ʬ
                  09  MDCREQ-MAIN-ASSISTANCE         PIC X(01).
      *           �ܿͲ�²��ʬ
                  09  MDCREQ-MAIN-FAMILY             PIC X(01).
      *           ���ݸ���̾
                  09  MDCREQ-MAIN-POLICYHOLDER       PIC X(100).
      *           ͭ��������
                  09  MDCREQ-MAIN-START-DATE         PIC X(10).
      *           ͭ����λ��
                  09  MDCREQ-MAIN-END-DATE           PIC X(10).
      *           �������
                  09  MDCREQ-PUBLIC-INSURANCE-INF  OCCURS  4 TIMES.
      *             ����μ���
                    11  MDCREQ-INSURANCE-CLASS       PIC X(03).
      *             ����μ���̾��
                    11  MDCREQ-INSURANCE-NAME        PIC X(20).
      *             ��ô���ֹ�
                    11  MDCREQ-PROVIDER-NUMBER       PIC X(08).
      *             ������ֹ�
                    11  MDCREQ-RECIPIENT-NUMBER      PIC X(20).
      *             ͭ��������
                    11  MDCREQ-INSURANCE-START-DATE  PIC X(10).
      *             ͭ����λ��
                    11  MDCREQ-INSURANCE-END-DATE    PIC X(10).
      *
      *         ���ž���
                07  MDCREQ-MDC-INFO.
      *             ���ֳ���ʬ
                    09  MDCREQ-OFFTIME               PIC X(1).
      *             �ǻ�����ʬ
                    09  MDCREQ-DOCTORS-FEE           PIC X(2).
      *
      *             ���ż��̶�ʬ
                    09  MDCREQ-MDC-CLASS             PIC X(3).
      *             ���ż���̾��
                    09  MDCREQ-MDC-CLASS-NAME        PIC X(40).
      *             ���ť�����
                    09  MDCREQ-PRSCRPT-CODE          PIC X(9).
      *             ����̾��
                    09  MDCREQ-PRSCRPT-NAME          PIC X(140).
      *
