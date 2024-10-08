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
      ******************************************************************
      *    ���Уɥ쥹�ݥ󥹼��հ���           COPY (CPACCPTLSTRES.INC)
      ******************************************************************
        01  XML-ACCEPTLSTRES.
            02  ACPLSTRES-CONTEXT          PIC S9(9)   BINARY.
            02  ACPLSTRES-OBJECT           PIC X(36).
            02  ACPLSTRES-MODE             PIC S9(9)   BINARY.
            02  ACPLSTRES-RECORDNAME       PIC X(128).
            02  ACPLSTRES-APPOINTRES.
      *       �»���
              03  ACPLSTRES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  ACPLSTRES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  ACPLSTRES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  ACPLSTRES-API-RESULT-MSG      PIC X(100).
      *
              03  ACPLSTRES-RESKEY              PIC X(100).
      *       ������
              03  ACPLSTRES-ACCEPT-DATE         PIC X(10).
      *       ��������
              03  ACPLSTRES-ACCEPT-LST-G.
      *R02.12     05  ACPLSTRES-ACCEPT-LST      OCCURS   500.
      *R05.01     05  ACPLSTRES-ACCEPT-LST      OCCURS   1000.
                  05  ACPLSTRES-ACCEPT-LST      OCCURS   1500.
      *               ���ջ���
                      07  ACPLSTRES-ACCEPT-TIME        PIC X(8).
      *               ���գɣ�
                      07  ACPLSTRES-ACCEPT-ID          PIC X(5).
      *               ���Ų�
                      07  ACPLSTRES-ACCEPT-DEP-CODE    PIC X(2).
      *               ���Ų�̾��
                      07  ACPLSTRES-ACCEPT-DEP-NAME    PIC X(80).
      *               �ɥ�����������
                      07  ACPLSTRES-ACCEPT-DR-CODE     PIC X(5).
      *               �ɥ�������̾
                      07  ACPLSTRES-ACCEPT-DR-NAME     PIC X(80).
      *               �������ƶ�ʬ
                      07  ACPLSTRES-ACCEPT-DEP-INFO    PIC X(2).
      *               ����������
                      07  ACPLSTRES-CLAIM-INFO         PIC X(1).
      *               ��׻���
                      07  ACPLSTRES-ACCOUNT-TIME       PIC X(8).
      *H29.4
      *               ͽ�����
                      07  ACPLSTRES-APPOINT-TIME       PIC X(8).
      *               ͽ��ID
                      07  ACPLSTRES-APPOINT-ID         PIC X(2).
      *               ���Ծ���
                      07  ACPLSTRES-PATIENT-INFOMATION.
      *                   �����ֹ�
                          09  ACPLSTRES-PATIENTID         PIC X(20).
      *                   ���Ի�̾
                          09  ACPLSTRES-NAME              PIC X(100).
      *                   ���ԥ��ʻ�̾
                          09  ACPLSTRES-KANANAME          PIC X(100).
      *                   ������
                          09  ACPLSTRES-BIRTHDAY          PIC X(10).
      *                   ����
                          09  ACPLSTRES-SEX               PIC X(1).
      *               �ݸ�����
                      07  ACPLSTRES-INSURANCE-INFO.
      *                 �ݸ��ȹ礻�ֹ�
                        09  ACPLSTRES-COMBINATION-NUMBER     PIC X(04).
      *H29.4            ��ɽ����ʬ
                        09  ACPLSTRES-COMBINATION-HYOJI      PIC X(01).
      *                 �ݸ��μ���
                        09  ACPLSTRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *                 �ݸ����ֹ�
                        09  ACPLSTRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *                 �ݸ��μ���̾��
                        09  ACPLSTRES-MAIN-INSURANCE-NAME    PIC X(20).
      *                 ����
                        09  ACPLSTRES-MAIN-MARK              PIC X(80).
      *                 �ֹ�
                        09  ACPLSTRES-MAIN-NUMBER            PIC X(80).
      *R02.12
      *                 ����
                        09  ACPLSTRES-MAIN-EDABAN             PIC X(02).
      *                 ��³��ʬ
                        09  ACPLSTRES-MAIN-CONTINUATION      PIC X(01).
      *                 �����ʬ
                        09  ACPLSTRES-MAIN-ASSISTANCE        PIC X(01).
      *                 �ܿͲ�²��ʬ
                        09  ACPLSTRES-MAIN-FAMILY            PIC X(01).
      *                 ���ݸ���̾
                        09  ACPLSTRES-MAIN-POLICYHOLDER      PIC X(100).
      *                 ͭ��������
                        09  ACPLSTRES-MAIN-START-DATE        PIC X(10).
      *                 ͭ����λ��
                        09  ACPLSTRES-MAIN-END-DATE          PIC X(10).
      *                 �������
                        09  ACPLSTRES-PUBLIC-INSURANCE-INFO           
                                                  OCCURS  4 TIMES.
      *                    ����μ���
                           11  ACPLSTRES-INSURANCE-CLASS     PIC X(03).
      *                    ����μ���̾��
                           11  ACPLSTRES-INSURANCE-NAME      PIC X(20).
      *                    ��ô���ֹ�
                           11  ACPLSTRES-PROVIDER-NUMBER     PIC X(08).
      *                    ������ֹ�
                           11  ACPLSTRES-RECIPIENT-NUMBER    PIC X(20).
      *                    ��������ôΨ�ʳ��
                           11  ACPLSTRES-RATE-ADMISSION      PIC X(04).
      *                    �����ݸ����
                           11  ACPLSTRES-MONEY-ADMISSION     PIC X(06). 
      *                    �������ôΨ�ʳ��
                           11  ACPLSTRES-RATE-OUTPATIENT     PIC X(04).
      *                    ����ݸ����
                           11  ACPLSTRES-MONEY-OUTPATIENT    PIC X(06).
      *                    ͭ��������
                           11  ACPLSTRES-INSURANCE-START-DATE
                                                           PIC X(10).
      *                    ͭ����λ��
                           11  ACPLSTRES-INSURANCE-END-DATE  PIC X(10).
      *
      *H30.11
      *               �����Ͽ����
                      07  ACPLSTRES-MEMO-INF.
                          09  ACPLSTRES-MEMO-00   PIC X(01).
                          09  ACPLSTRES-MEMO-KA   PIC X(01).
      *R01.5
      *               ���̼��ս�
                      07  ACPLSTRES-DISPLAY-SORT.
                          09  ACPLSTRES-SORT-NUMBER      PIC X(4).
      *                   ���ս��ѹ���ʬ
                          09  ACPLSTRES-SORT-MARK        PIC X(5).
