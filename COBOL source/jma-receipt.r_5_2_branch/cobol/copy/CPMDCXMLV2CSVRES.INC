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
      ******************************************************************
      *    ���Уɡ��Żҽ���䵣ãӣ־���(xml2)   COPY (CPMDCXMLV2CSVRES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
        01  XML-MEDICALCSVRES.
            02  MDCCSV-CONTEXT             PIC S9(9)   BINARY.
            02  MDCCSV-OBJECT              PIC X(36).
            02  MDCCSV-MODE                PIC S9(9)   BINARY.
            02  MDCCSV-RECORDNAME          PIC X(128).
            02  MDCCSV-MEDICALRES.
      *       �»���
              03  MDCCSV-INFORMATION-DATE  PIC X(10).
      *       �»ܻ���
              03  MDCCSV-INFORMATION-TIME  PIC X(8).
      *       �ֵѥ�����
              03  MDCCSV-API-RESULT        PIC X(2).
      *       �ֵѥ�å�����
              03  MDCCSV-API-RESULT-MSG    PIC X(100).
      *
              03  MDCCSV-RESKEY            PIC X(100).
      *       ��������
              03  MDCCSV-PERFORM-DATE      PIC X(10).
              03  MDCCSV-PERFORM-TIME      PIC X(8).
      *
              03  MDCCSV-APPOINT-DEP-CODE  PIC X(2).
              03  MDCCSV-APPOINT-DEP-NAME  PIC X(80).
              03  MDCCSV-APPOINT-DR-CODE   PIC X(5).
              03  MDCCSV-APPOINT-DR-NAME   PIC X(80).
              03  MDCCSV-PATIENT-INFOMATION.
                04  MDCCSV-PATIENTID       PIC X(20).
                04  MDCCSV-NAME            PIC X(100).
                04  MDCCSV-KANANAME        PIC X(100).
                04  MDCCSV-BIRTHDAY        PIC X(10).
                04  MDCCSV-SEX             PIC X(1).
      *         �ݸ�����
                04  MDCCSV-INSURANCE-G.
      *             �ݸ��ȹ礻�ֹ�
                    06  MDCCSV-COMBINATION-NUMBER     PIC X(04).
      *             �ݸ��μ���
                    06  MDCCSV-MAIN-INSURANCE-CLASS   PIC X(03).
      *             �ݸ����ֹ�
                    06  MDCCSV-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             �ݸ��μ���̾��
                    06  MDCCSV-MAIN-INSURANCE-NAME    PIC X(20).
      *             ����
                    06  MDCCSV-MAIN-MARK              PIC X(80).
      *             �ֹ�
                    06  MDCCSV-MAIN-NUMBER            PIC X(80).
      *             ����
                    06  MDCCSV-MAIN-EDABAN            PIC X(02).
      *             ��³��ʬ
                    06  MDCCSV-MAIN-CONTINUATION      PIC X(01).
      *             �����ʬ
                    06  MDCCSV-MAIN-ASSISTANCE        PIC X(01).
      *             �ܿͲ�²��ʬ
                    06  MDCCSV-MAIN-FAMILY            PIC X(01).
      *             ���ݸ���̾
                    06  MDCCSV-MAIN-POLICYHOLDER      PIC X(100).
      *             ͭ��������
                    06  MDCCSV-MAIN-START-DATE        PIC X(10).
      *             ͭ����λ��
                    06  MDCCSV-MAIN-END-DATE          PIC X(10).
      *             �������
                    06  MDCCSV-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               ����μ���
                      07  MDCCSV-INSURANCE-CLASS         PIC X(03).
      *               ����μ���̾��
                      07  MDCCSV-INSURANCE-NAME          PIC X(20).
      *               ��ô���ֹ�
                      07  MDCCSV-PROVIDER-NUMBER         PIC X(08).
      *               ������ֹ�
                      07  MDCCSV-RECIPIENT-NUMBER        PIC X(20).
      *               ��������ôΨ�ʳ��
                      07  MDCCSV-RATE-ADMISSION          PIC X(04).
      *               �����ݸ����
                      07  MDCCSV-MONEY-ADMISSION         PIC X(06).
      *               �������ôΨ�ʳ��
                      07  MDCCSV-RATE-OUTPATIENT         PIC X(04).
      **              ����ݸ����
                      07  MDCCSV-MONEY-OUTPATIENT        PIC X(06).
      *               ͭ��������
                      07  MDCCSV-INSURANCE-START-DATE    PIC X(10).
      *               ͭ����λ��
                      07  MDCCSV-INSURANCE-END-DATE      PIC X(10).
      *       ���ӥǡ�����å�����
              03  MDCCSV-MEDICAL-MSG1-G.
      *           ���顼������
                  05  MDCCSV-MEDI-RESULT        PIC X(4).
      *           ���顼��å�����
                  05  MDCCSV-MEDI-RESULT-MSG1   PIC X(100).
                  05  MDCCSV-MEDI-RESULT-MSG2   PIC X(100).
      *�Żҽ���䵾���
              03  MDCCSV-EPRECSV-G.
                  05  MDCCSV-CSVDATA             PIC X(250000).
                  05  MDCCSV-CSVDATA2            PIC X(250000).
                  05  MDCCSV-CSVDATA3            PIC X(250000).
                  05  MDCCSV-CSVDATA4            PIC X(250000).
                  05  MDCCSV-CSVDATA5            PIC X(250000).
      *!!!
