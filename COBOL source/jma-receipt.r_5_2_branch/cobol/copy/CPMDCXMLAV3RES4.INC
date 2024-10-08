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
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 05.00.00     NACL-¿��Ǽ  20/03/XX  �ң���ǯ��������б�
      *****************************************************************
      ******************************************************************
      *    ���Уɥ쥹�ݥ󥹿��Ź԰���Ͽ���� (���β�)
      *                                      COPY (CPMDCXMLAV3RE4.INC)
      *         �������Ź԰ٺ��
      ******************************************************************
       01  XML-MEDICALAV3RES4.
            03  MDCRES-CONTEXT             PIC S9(9)   BINARY.
            03  MDCRES-OBJECT              PIC X(36).
            03  MDCRES-MODE                PIC S9(9)   BINARY.
            03  MDCRES-RECORDNAME          PIC X(128).
            03  MDCRES-MEDICALRES.
      *        �ꥯ�������ֹ�
               05  MDCRES-REQUEST-NUMBER    PIC X(2).
      *        �쥹�ݥ��ֹ�
               05  MDCRES-RESPONSE-NUMBER   PIC X(2).
      *        UID���Żҥ���ơ�
               05  MDCRES-KARTE-UID         PIC X(36).
      *        UID�ʥ��륫��
               05  MDCRES-ORCA-UID          PIC X(36).
      *        �»���
               05  MDCRES-INFORMATION-DATE  PIC X(10).
      *        �»ܻ���
               05  MDCRES-INFORMATION-TIME  PIC X(8).
      *        �ֵѥ�����
               05  MDCRES-API-RESULT        PIC X(3).
      *        �ֵѥ�å�����
               05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
               05  MDCRES-RESKEY            PIC X(100).
      *        ���Ծ���
               05  MDCRES-PATIENTID        PIC X(20).
               05  MDCRES-NAME             PIC X(100).
               05  MDCRES-KANANAME         PIC X(100).
               05  MDCRES-BIRTHDAY         PIC X(10).
               05  MDCRES-SEX              PIC X(1).
      *        �����ֵѤ���
               05  MDCRES-MEDICAL-SELECT-FLAG   PIC X(05).
      *        ��������
               05  MDCRES-PERFORM-DATE      PIC X(10).
      *        ���Ų�
               05  MDCRES-APPOINT-DEP-CODE  PIC X(2).
               05  MDCRES-APPOINT-DEP-NAME  PIC X(80).
      *        Ϣ��(Ʊ��������ʬ)
               05  MDCRES-NUMBER            PIC X(01).
      *        ������
               05  MDCRES-NYUIN-DATE        PIC  X(10).
      *        �ౡ��
               05  MDCRES-TAIIN-DATE        PIC  X(10).
      *        �ɥ�����
      *        05  MDCRES-APPOINT-DR-CODE   PIC X(5).
      *        05  MDCRES-APPOINT-DR-NAME   PIC X(80).
      *        �ݸ�����
               05  MDCRES-INSURANCE-G.
      *            �ݸ��ȹ礻�ֹ�
                   09  MDCRES-COMBINATION-NUMBER     PIC X(04).
      *            �ݸ��ȹ礻��ô���
                   09  MDCRES-COMBINATION-RATE       PIC X(03).
      *            �ݸ��μ���
                   09  MDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *            �ݸ����ֹ�
                   09  MDCRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *            �ݸ��μ���̾��
                   09  MDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *            ����
                   09  MDCRES-MAIN-MARK              PIC X(80).
      *            �ֹ�
                   09  MDCRES-MAIN-NUMBER            PIC X(80).
      *R03.01  ����
                   09  MDCRES-MAIN-EDABAN            PIC X(02).
      *            ��³��ʬ
                   09  MDCRES-MAIN-CONTINUATION      PIC X(01).
      *            �����ʬ
                   09  MDCRES-MAIN-ASSISTANCE        PIC X(01).
      *            �����ʬ̾��
                   09  MDCRES-MAIN-ASSI-NAME         PIC X(20).
      *            �ܿͲ�²��ʬ
                   09  MDCRES-MAIN-FAMILY            PIC X(01).
      *            �������
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
      *               �������ôΨ�ʳ��
      *               11  MDCRES-RATE-OUTPATIENT         PIC X(04).
      *               ����ݸ����
      *               11  MDCRES-MONEY-OUTPATIENT        PIC X(06).
      *
      *        ���������ֵ�
               05  MDCRES-MEDICAL-INF-G.
                 07  MDCRES-MDC-INFO          OCCURS   120.
      *             ���Ŷ�ʬ
                    09  MDCRES-MDC-CLASS-KBN         PIC X(2).
      *             ���ż��̶�ʬ
                    09  MDCRES-MDC-CLASS             PIC X(3).
      *             ���ż���̾��
                    09  MDCRES-MDC-CLASS-NAME        PIC X(40).
      *             ���
                    09  MDCRES-MDC-CLASS-NUMBER      PIC X(03).
      *             ������
                    09  MDCRES-MDC-POINT             PIC X(08).
      *             �޶��
                    09  MDCRES-MDC-MONEY             PIC X(08).
      *             ����ʬ��True�����ޡ�
                    09  MDCRES-INCLUSION-CLASS       PIC X(05).
      *
      *             ��縡���θ�����
                    09  MDCRES-HKTKNSA-CNT           PIC X(02).
      *
                    09  MDCRES-PRSCRPT-INFO-G.
                      10  MDCRES-PRESCRIPTION-INFO    OCCURS   50.
      *                 ���ť�����
                        11  MDCRES-PRSCRPT-CODE          PIC X(9).
      *                 ����̾��
                        11  MDCRES-PRSCRPT-NAME          PIC X(140).
      *R02.6
      *                 ���ϥ�����
                        11  MDCRES-PRSCRPT-NAME-INPUT    PIC X(200).
      *                 ����
                        11  MDCRES-PRSCRPT-NUMBER        PIC X(11).
      *                 ñ�̥�����
                        11  MDCRES-PRSCRPT-TANICD        PIC X(03).
      *                 ñ��̾��
                        11  MDCRES-PRSCRPT-TANICD-NAME   PIC X(24).
      *                 ������
                        11  MDCRES-PRSCRPT-MONEY         PIC X(07).
      *                 ��������������
                        11  MDCRES-PRSCRPT-ATAI-G.
                            13  MDCRES-PRSCRPT-ATAI      PIC X(10)
                                                         OCCURS   5.
      *                 �ե����ʬ���
                        11  MDCRES-PRSCRPT-FILMNUM       PIC X(03).
      *                 ����̾���ܶ�ʬ�ʣ�������̾����������̾�ʰ���̾��
      *                 11  MDCRES-PRSCRPT-GENERIC       PIC X(01).
      *                 ����̾̾��
      *                 11  MDCRES-PRSCRPT-GENNAME       PIC X(200).
      *                 ��³�ؼ���ʬ
                        11  MDCRES-PRSCRPT-CONTKBN       PIC X(01).
      *                 ����������ؼ���ʬ
                        11  MDCRES-PRSCRPT-NAIFKBN       PIC X(01).
      *
      *                 ��ư����ʤ���ʬ�ʺ������ܼ��Ӳû���
                        11  MDCRES-PRSCRPT-NOAUTO        PIC X(03).
      *
      *                 ��ư��ʬ
                        11  MDCRES-PRSCRPT-AUTOKBN       PIC X(01).
      *
      *        �������ƥ��顼
               05  MDCRES-MEDICAL-MSG1-G.
      *           ��˥󥰥�å�����
                  07  MDCRES-MEDI-WARNING-TBL    OCCURS  40.
      *               ��˥󥰥�����
                      09  MDCRES-MEDI-WARNING        PIC X(03).
      *               ��˥󥰥�å�����
                      09  MDCRES-MEDI-WARNING-MSG    PIC X(100).
      *
      *        ��ǧ��å����������ɡ����������ɡ�
               05  MDCRES-SELECT-CODE         PIC X(04).
      *        ��ǧ��å���������
               05  MDCRES-SELECT-MSG          PIC X(100).
      *
