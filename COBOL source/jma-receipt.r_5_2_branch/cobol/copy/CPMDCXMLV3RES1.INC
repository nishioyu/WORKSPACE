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
      *    ���Уɥ쥹�ݥ󥹿��Ź԰����Ͼ��� (���β�)   COPY (CPMDCXMLV3RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 05.00.00     NACL-¿��Ǽ  20/03/XX  �ң���ǯ��������б�
      *****************************************************************
        01  XML-MEDICALV3RES1.
            03  MDCRES-CONTEXT             PIC S9(9)   BINARY.
            03  MDCRES-OBJECT              PIC X(36).
            03  MDCRES-MODE                PIC S9(9)   BINARY.
            03  MDCRES-RECORDNAME          PIC X(128).
            03  MDCRES-MEDICALRES.
      *       �ꥯ�������ֹ�
              05  MDCRES-REQUEST-NUMBER    PIC X(2).
      *       �쥹�ݥ��ֹ�
              05  MDCRES-RESPONSE-NUMBER   PIC X(2).
      *       UID���Żҥ���ơ�
              05  MDCRES-KARTE-UID         PIC X(36).
      *       UID�ʥ��륫��
              05  MDCRES-ORCA-UID          PIC X(36).
      *       �»���
              05  MDCRES-INFORMATION-DATE  PIC X(10).
      *       �»ܻ���
              05  MDCRES-INFORMATION-TIME  PIC X(8).
      *       �ֵѥ�����
              05  MDCRES-API-RESULT        PIC X(03).
      *       �ֵѥ�å�����
              05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
              05  MDCRES-RESKEY            PIC X(100).
      *       ��������
              05  MDCRES-PERFORM-DATE      PIC X(10).
              05  MDCRES-PERFORM-TIME      PIC X(8).
      *       ���ӥǡ���¸�߶�ʬ
              05  MDCRES-TEMP-MEDICAL-FLAG PIC X(05).
      *       ��ǻ������ʤ���ʬ
              05  MDCRES-DOCTORS-FEE-INITIAL   PIC X(05).
      *       ʣ���ֵѤ���
              05  MDCRES-MEDICAL-SELECT-FLAG   PIC X(05).
      *       �ǽ��象�����ౡ����
              05  MDCRES-LAST-DATE             PIC  X(10).
      *       ���Ų�
              05  MDCRES-APPOINT-DEP-CODE  PIC X(2).
              05  MDCRES-APPOINT-DEP-NAME  PIC X(80).
      *       �ɥ�����
              05  MDCRES-APPOINT-DR-CODE   PIC X(5).
              05  MDCRES-APPOINT-DR-NAME   PIC X(80).
      *
      *       ��������ǻ���
              05  MDCRES-IN-MEDICAL-INF-G.
      *           ���ż��̶�ʬ
                  07  MDCRES-IN-MDC-CLASS        PIC X(3).
      *           ���ż���̾��
                  07  MDCRES-IN-MDC-CLASS-NAME   PIC X(40).
      *           ���ť�����
                  07  MDCRES-IN-PRSCRPT-CODE     PIC X(9).
      *           ����̾��
                  07  MDCRES-IN-PRSCRPT-NAME     PIC X(140).
      *
      *
              05  MDCRES-PATIENT-INFOMATION.
      *         ���Ծ���
                07  MDCRES-PATIENTID       PIC X(20).
                07  MDCRES-NAME            PIC X(100).
                07  MDCRES-KANANAME        PIC X(100).
                07  MDCRES-BIRTHDAY        PIC X(10).
                07  MDCRES-SEX             PIC X(1).
      *         �ݸ�����
                07  MDCRES-INSURANCE-G.
      *             �ݸ��ȹ礻�ֹ�
                    09  MDCRES-COMBINATION-NUMBER     PIC X(04).
      *             �ݸ��μ���
                    09  MDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             �ݸ����ֹ�
                    09  MDCRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             �ݸ��μ���̾��
                    09  MDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             ����
                    09  MDCRES-MAIN-MARK              PIC X(80).
      *             �ֹ�
                    09  MDCRES-MAIN-NUMBER            PIC X(80).
      *R03.01  ����
                    09  MDCRES-MAIN-EDABAN            PIC X(02).
      *             ��³��ʬ
                    09  MDCRES-MAIN-CONTINUATION      PIC X(01).
      *             �����ʬ
                    09  MDCRES-MAIN-ASSISTANCE        PIC X(01).
      *             �����ʬ
                    09  MDCRES-MAIN-ASSI-NAME         PIC X(20).
      *             �ܿͲ�²��ʬ
                    09  MDCRES-MAIN-FAMILY            PIC X(01).
      *             ���ݸ���̾
                    09  MDCRES-MAIN-POLICYHOLDER      PIC X(100).
      *             ͭ��������
                    09  MDCRES-MAIN-START-DATE        PIC X(10).
      *             ͭ����λ��
                    09  MDCRES-MAIN-END-DATE          PIC X(10).
      *             �������
                    09  MDCRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               ����μ���
                      11  MDCRES-INSURANCE-CLASS         PIC X(03).
      *               ����μ���̾��
                      11  MDCRES-INSURANCE-NAME          PIC X(20).
      *               ��ô���ֹ�
                      11  MDCRES-PROVIDER-NUMBER         PIC X(08).
      *               ������ֹ�
                      11  MDCRES-RECIPIENT-NUMBER        PIC X(20).
      *               ��������ôΨ�ʳ��
                      11  MDCRES-RATE-ADMISSION          PIC X(04).
      *               �����ݸ����
                      11  MDCRES-MONEY-ADMISSION         PIC X(06).
      *               �������ôΨ�ʳ��
                      11  MDCRES-RATE-OUTPATIENT         PIC X(04).
      *               ����ݸ����
                      11  MDCRES-MONEY-OUTPATIENT        PIC X(06).
      *               ͭ��������
                      11  MDCRES-INSURANCE-START-DATE    PIC X(10).
      *               ͭ����λ��
                      11  MDCRES-INSURANCE-END-DATE      PIC X(10).
      *
      *        �ֵѻ��ֳ���ʬ
               05  MDCRES-OFFTIME               PIC X(01).
      *        �ֵѿǻ���
               05  MDCRES-MEDICAL-INF-G.
                   07  MDCRES-MEDICAL-INF        OCCURS   10.
                     08  MDCRES-MEDICAL-INF2       OCCURS   10.
      *               ���ż��̶�ʬ
                      09  MDCRES-MDC-CLASS             PIC X(3).
      *               ���ż���̾��
                      09  MDCRES-MDC-CLASS-NAME        PIC X(40).
      *               ���Ų���ʣ��Τߡ�
                      09  MDCRES-MDC-CLASS-NUMBER      PIC X(03).
      *               ������
                      09  MDCRES-PRESCRIPTION-INFO     OCCURS  10 TIMES.
      *                 ���ť�����
                        11  MDCRES-PRSCRPT-CODE          PIC X(9).
      *                 ����̾�Ρʥ����ȡ�
                        11  MDCRES-PRSCRPT-NAME          PIC X(140).
      *                 ���̡ʣ��Τߡ�
                        11  MDCRES-PRSCRPT-NUMBER        PIC X(11).
      *                 ¾�ʼ��Ǥο��Ų�
      ******            11  MDCRES-DEP-CODE              PIC X(02).
      *
              05  MDCRES-MEDICAL-MSG1-G.
      *           ���顼������
                  07  MDCRES-MEDI-RESULT        PIC X(3).
      *           ���顼��å�����
                  07  MDCRES-MEDI-RESULT-MSG    PIC X(100).
      *           ��˥󥰥�å�����
                  07  MDCRES-MEDI-WARNING-TBL   OCCURS  50.
      *               ��˥󥰥�����
                      09  MDCRES-MEDI-WARNING        PIC X(03).
      *               ��˥󥰥�å�����
                      09  MDCRES-MEDI-WARNING-MSG    PIC X(100).
      *               ��˥󥰥�����
                      09  MDCRES-MEDI-CODE           PIC X(09).
      *
      *��Ͽ�Ѥ����ӥǡ���
      *        ���������ֵ�
               05  MDCRES-WKMEDICAL-INF-G.
                 07  MDCRES-WKMDC-INFO          OCCURS   110.
      *             ���Ŷ�ʬ
                    09  MDCRES-WKMDC-CLASS-KBN       PIC X(2).
      *             ���ż��̶�ʬ
                    09  MDCRES-WKMDC-CLASS           PIC X(3).
      *             ���ż���̾��
                    09  MDCRES-WKMDC-CLASS-NAME      PIC X(40).
      *             ���
                    09  MDCRES-WKMDC-CLASS-NUMBER    PIC X(03).
      *
                    09  MDCRES-WKPRSCRPT-INFO-G.
                      10  MDCRES-WKPRESCRIPTION-INFO    OCCURS   50.
      *                 ���ť�����
                        11  MDCRES-WKPRSCRPT-CODE          PIC X(9).
      *                 ����̾��
                        11  MDCRES-WKPRSCRPT-NAME          PIC X(140).
      *                 ����
                        11  MDCRES-WKPRSCRPT-NUMBER        PIC X(11).
      *                 ������
                        11  MDCRES-WKPRSCRPT-MONEY         PIC X(07).
      *                 ��������������
                        11  MDCRES-WKPRSCRPT-ATAI-G.
                            13  MDCRES-WKPRSCRPT-ATAI      PIC X(10)
                                                         OCCURS   5.
      *                 ��³�ؼ���ʬ
                        11  MDCRES-WKPRSCRPT-CONTKBN       PIC X(01).
      *                 ����������ؼ���ʬ
                        11  MDCRES-WKPRSCRPT-NAIFKBN       PIC X(01).
      *                 ��ư��ʬ
                        11  MDCRES-WKPRSCRPT-AUTOKBN       PIC X(01).
      *
      *
