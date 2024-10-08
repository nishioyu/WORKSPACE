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
      *    ���Уɥ쥹�ݥ�ͽ����� (xml2)    COPY (CPACCPTLSTV2RES.INC)
      ******************************************************************
        01  XML-APPOINTLSTRES.
            02  APLSTRES-CONTEXT           PIC S9(9)   BINARY.
            02  APLSTRES-OBJECT            PIC X(36).
            02  APLSTRES-MODE              PIC S9(9)   BINARY.
            02  APLSTRES-RECORDNAME        PIC X(128).
            02  APLSTRES-APPOINTRES.
      *       �»���
              03  APLSTRES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  APLSTRES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  APLSTRES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  APLSTRES-API-RESULT-MSG      PIC X(100).
      *
              03  APLSTRES-RESKEY              PIC X(100).
      *       ͽ����
              03  APLSTRES-APPOINT-DATE        PIC X(10).
      *       ͽ������
              03  APLSTRES-APPOINT-LST-G.
                  05  APLSTRES-APPOINT-LST     OCCURS   500.
      *               ͽ�����
                      07  APLSTRES-APPOINT-TIME        PIC X(8).
      *               �������ƶ�ʬ
                      07  APLSTRES-APPOINT-DEP-INFO    PIC X(2).
      *               ���Ų�
                      07  APLSTRES-APPOINT-DEP-CODE    PIC X(2).
      *               ���Ų�̾��
                      07  APLSTRES-APPOINT-DEP-NAME    PIC X(80).
      *               �ɥ�����������
                      07  APLSTRES-APPOINT-DR-CODE     PIC X(5).
      *               �ɥ�������̾
                      07  APLSTRES-APPOINT-DR-NAME     PIC X(80).
      *               �象�Ѷ�ʬ
                      07  APLSTRES-VISIT-INFO          PIC X(1).
      *H29.11         ͽ��ID
                      07  APLSTRES-APPOINT-ID          PIC X(2).
      *               ���Ծ���
                      07  APLSTRES-PATIENT-INFOMATION.
      *                   �����ֹ�
                          09  APLSTRES-PATIENTID         PIC X(20).
      *                   ���Ի�̾
                          09  APLSTRES-NAME              PIC X(100).
      *                   ���ԥ��ʻ�̾
                          09  APLSTRES-KANANAME          PIC X(100).
      *                   ������
                          09  APLSTRES-BIRTHDAY          PIC X(10).
      *                   ����
                          09  APLSTRES-SEX               PIC X(1).
      *                   ���������ֹ�����
                          09  APLSTRES-HOME-TEL          PIC X(15).
      *2023/12
      *                   �ݸ�����
                          09  APLSTRES-MAIN-INSURANCE-G.
      *                       �ݸ��μ���
                              11  APLSTRES-MAIN-CLASS  PIC X(03).
      *                       �ݸ����ֹ�
                              11  APLSTRES-INSURANCE-NUMBER PIC X(08).
      *                       �ݸ��μ���̾��
                              11  APLSTRES-MAIN-CLASS-NAME  PIC X(20).
      *                       ����
                              11  APLSTRES-MAIN-MARK       PIC X(80).
      *                       �ֹ�
                              11  APLSTRES-MAIN-NUMBER     PIC X(80).
      *                       ����
                              11  APLSTRES-MAIN-EDABAN     PIC X(02).
      *                       �����ʬ
                              11  APLSTRES-MAIN-ASSISTANCE PIC X(01).
      *                       �����ʬ̾��
                              11  APLSTRES-MAIN-ASSI-NAME  PIC X(20).
      *                       �ܿͲ�²��ʬ
                              11  APLSTRES-MAIN-FAMILY     PIC X(01).
      *                       ͭ��������
                              11  APLSTRES-MAIN-START-DATE PIC X(10).
      *                       ͭ����λ��
                              11  APLSTRES-MAIN-END-DATE   PIC X(10).
      *                   �������
                          09  APLSTRES-PUBLIC-INSURANCE-INFG.
                              11  APLSTRES-PUBLIC-INSURANCE-INF
                                                      OCCURS  6  TIMES.
      *                       ����μ���
                                  13  APLSTRES-INSURANCE-CLASS
                                                           PIC X(03).
      *                       ����μ���̾��
                                  13  APLSTRES-PUBLIC-NAME
                                                           PIC X(20).
      *                       ��ô���ֹ�
                                  13  APLSTRES-PROVIDER-NUMBER
                                                           PIC X(08).
      *                       ������ֹ�
                                  13  APLSTRES-RECIPIENT-NUMBER
                                                          PIC X(20).
      *                        ͭ��������
                                  13  APLSTRES-INSURANCE-START-DATE
                                                          PIC X(10).
      *                       ͭ����λ��
                                  13  APLSTRES-INSURANCE-END-DATE
                                                          PIC X(10).
      *
