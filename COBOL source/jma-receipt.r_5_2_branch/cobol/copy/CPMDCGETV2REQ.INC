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
      * 04.07.00     NACL-¿��Ǽ  14/12/XX  ���ŷ���б�
      * 05.00.00     NACL-¿��Ǽ  17/08/XX  ��ɼ�ֹ��ɲ��б�
      * 05.02.00     ORCAMO       24/07/XX  ���������ɲ��б�
      *****************************************************************
      ******************************************************************
      *    ���Уɥꥯ�����ȿ��Ź԰پ������ (xml2)
      *                                        COPY (CPMDCGETREQ.INC)
      ******************************************************************
        01  XML-MEDICALGETREQ.
            02  MDCREQ-CONTEXT           PIC S9(9)   BINARY.
            02  MDCREQ-OBJECT            PIC X(36).
            02  MDCREQ-MODE              PIC S9(9)   BINARY.
            02  MDCREQ-RECORDNAME        PIC X(128).
            02  MDCREQ-MEDICALREQ.
      *       ������ʬ�ʣɡ�������
              03  MDCREQ-INOUT-CLASS       PIC X(1).
      *       �����ֹ�
              03  MDCREQ-PATIENTID         PIC X(20).
      *       ��������
              03  MDCREQ-PERFORM-DATE      PIC X(10).
      *       ���ŷ��
              03  MDCREQ-FOR-MONTHS        PIC X(02).
      *R06.7
      *       ���������ʼ��������
              03  MDCREQ-START-DATE       PIC X(10).
              03  MDCREQ-START-TIME       PIC X(08).
      *
              03  MDCREQ-MEDICAL-INFO.
      *         ���Ų�
                04  MDCREQ-DEP-CODE                PIC X(02).
      *         Ϣ��
                04  MDCREQ-NUMBER                  PIC X(01).
      *         �ݸ��ȹ礻�ֹ�
                04  MDCREQ-COMBINATION-NUMBER      PIC X(04).
      *         �ݸ��ȹ礻����
                04  MDCREQ-INSURANCE-INFO.
      *           �ݸ��μ���
                  05  MDCREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *           �ݸ��μ���
                  05  MDCREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *           �ݸ����ֹ�
                  05  MDCREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *           ����
                  05  MDCREQ-MAIN-MARK               PIC X(80).
      *           �ֹ�
                  05  MDCREQ-MAIN-NUMBER             PIC X(80).
      *R03.01  ����
                  05  MDCREQ-MAIN-EDABAN             PIC X(02).
      *           �������
                  05  MDCREQ-PUBLIC-INSURANCE-INF  OCCURS  4 TIMES.
      *             ����μ���
                    06  MDCREQ-INSURANCE-CLASS       PIC X(03).
      *             ����μ���
                    06  MDCREQ-INSURANCE-NAME        PIC X(20).
      *             ��ô���ֹ�
                    06  MDCREQ-PROVIDER-NUMBER       PIC X(08).
      *             ������ֹ�
                    06  MDCREQ-RECIPIENT-NUMBER      PIC X(20).
      *         ��ɼ�ֹ�
                04  MDCREQ-INVOICD-NUMBER    PIC X(07).
      *H30.1
      *         �쥻�Űܹ�ʬ�о�
                04  MDCREQ-CONTAIN-MIGRATION     PIC X(05).
      *
