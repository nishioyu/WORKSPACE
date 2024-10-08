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
      *    ���Уɥ쥹�ݥ���ô��׻�����
      *                                 COPY (CPACSIMULATEV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
        01  XML-ACSIMULATERES.
            03  ACSRES-CONTEXT             PIC S9(9)   BINARY.
            03  ACSRES-OBJECT              PIC X(36).
            03  ACSRES-MODE                PIC S9(9)   BINARY.
            03  ACSRES-RECORDNAME          PIC X(128).
            03  ACSRES-MEDICALRES.
              05  ACSRES-INFORMATION-DATE  PIC X(10).
              05  ACSRES-INFORMATION-TIME  PIC X(8).
              05  ACSRES-API-RESULT        PIC X(2).
              05  ACSRES-API-RESULT-MSG    PIC X(100).
              05  ACSRES-RESKEY            PIC X(100).
              05  ACSRES-PERFORM-DATE      PIC X(10).
      *       ���ֳ���ʬ�ʿǻ�����
              05  ACSRES-TIME-CLASS        PIC X(1).
      *       ���Ų�
              05  ACSRES-APPOINT-DEP-CODE  PIC X(2).
              05  ACSRES-APPOINT-DEP-NAME  PIC X(80).
      *       ���Ծ���
              05  ACSRES-PATIENT-INFOMATION.
                07  ACSRES-PATIENTID       PIC X(20).
                07  ACSRES-NAME            PIC X(100).
                07  ACSRES-KANANAME        PIC X(100).
                07  ACSRES-BIRTHDAY        PIC X(10).
                07  ACSRES-SEX             PIC X(01).
                07  ACSRES-RATE            PIC X(12).
      *         �ݸ�����
                07  ACSRES-INSURANCE-G.
      *             �ݸ��ȹ礻�ֹ�
                    09  ACSRES-COMBINATION-NUMBER  PIC X(4).
      *             �ݸ��μ���
                    09  ACSRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             �ݸ����ֹ�
                    09  ACSRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             �ݸ��μ���̾��
                    09  ACSRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             ����
                    09  ACSRES-MAIN-MARK              PIC X(80).
      *             �ֹ�
                    09  ACSRES-MAIN-NUMBER            PIC X(80).
      *R03.01
      *             ����
                    09  ACSRES-MAIN-EDABAN            PIC X(02).
      *             ��³��ʬ
                    09  ACSRES-MAIN-CONTINUATION      PIC X(01).
      *             �����ʬ
                    09  ACSRES-MAIN-ASSISTANCE        PIC X(01).
      *             �����ʬ̾��
                    09  ACSRES-MAIN-ASSISTANCE-NAME   PIC X(10).
      *             �ܿͲ�²��ʬ
                    09  ACSRES-MAIN-FAMILY            PIC X(01).
      *             ���ݸ���̾
                    09  ACSRES-MAIN-POLICYHOLDER      PIC X(100).
      *             ͭ��������
                    09  ACSRES-MAIN-START-DATE        PIC X(10).
      *             ͭ����λ��
                    09  ACSRES-MAIN-END-DATE          PIC X(10).
      *             �������
                    09  ACSRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               ����μ���
                      11  ACSRES-INSURANCE-CLASS         PIC X(03).
      *               ����μ���̾��
                      11  ACSRES-INSURANCE-NAME          PIC X(20).
      *               ��ô���ֹ�
                      11  ACSRES-PROVIDER-NUMBER         PIC X(08).
      *               ������ֹ�
                      11  ACSRES-RECIPIENT-NUMBER        PIC X(20).
      *               ��������ôΨ�ʳ��
                      11  ACSRES-RATE-ADMISSION          PIC X(04).
      *               �����ݸ����
                      11  ACSRES-MONEY-ADMISSION         PIC X(06).
      *               �������ôΨ�ʳ��
                      11  ACSRES-RATE-OUTPATIENT         PIC X(04).
      *               ����ݸ����
                      11  ACSRES-MONEY-OUTPATIENT        PIC X(06).
      *               ͭ��������
                      11  ACSRES-INSURANCE-START-DATE    PIC X(10).
      *               ͭ����λ��
                      11  ACSRES-INSURANCE-END-DATE      PIC X(10).
      *
      *         ��ô�۾���
                07  ACSRES-SYUNOU-ARA.
      *             ������
                    09  ACSRES-SKYMONEY            PIC  X(10).
      *             �ݸ�Ŭ�Ѷ��
                    09  ACSRES-HKNTEKMONEY         PIC  X(10).
      *             ������
                    09  ACSRES-JIHIMONEY           PIC  X(10).
      *             ���ް�����ô��
                    09  ACSRES-YKZ-FTN             PIC  X(10).
      *             Ϸ�Ͱ�����ô��
                    09  ACSRES-RJN-FTN             PIC  X(10).
      *             ���������ô��
                    09  ACSRES-KOH-FTN             PIC  X(10).
      *             ϫ�Ҷ�۹��
                    09  ACSRES-RSI-TOTALMONEY      PIC  X(10).
      *         ������������
                07  ACSRES-SYUNOU-ARA.
      *             �ݸ��������
                    09  ACSRES-TOTAL-HKNTEN        PIC  X(10).
      *             �ݸ�����
                    09  ACSRES-HKNTEN-G.
                        11  ACSRES-HKNTEN-TBL      OCCURS   16.
                            13  ACSRES-HKNTEN-MEI      PIC  X(30).
                            13  ACSRES-HKNTEN          PIC  X(10).
      *
