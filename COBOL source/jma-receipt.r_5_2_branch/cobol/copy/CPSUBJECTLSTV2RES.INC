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
      *    ���Уɥ쥹�ݥ󥹾ɾ��ܵ���Ͽ�������(xml2)
      *                                    COPY (CPSUBJECTLSTV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
        01  XML-SUBJECTLSTRES.
            03  SUBJETRES-CONTEXT           PIC S9(9)   BINARY.
            03  SUBJETRES-OBJECT            PIC X(36).
            03  SUBJETRES-MODE              PIC S9(9)   BINARY.
            03  SUBJETRES-RECORDNAME        PIC X(128).
            03  SUBJETRES-MEDICALRES.
      *       �»���
              05  SUBJETRES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              05  SUBJETRES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              05  SUBJETRES-API-RESULT          PIC X(3).
      *       �ֵѥ�å�����
              05  SUBJETRES-API-RESULT-MSG      PIC X(100).
      *
              05  SUBJETRES-RESKEY              PIC X(100).
      *       ��Ͽ����
              05  SUBJETRES-PATIENT-INFOMATION.
      *           �����ֹ�
                  07  SUBJETRES-PATIENTID         PIC X(20).
      *           ���Ի�̾
                  07  SUBJETRES-NAME              PIC X(100).
      *           ���ԥ��ʻ�̾
                  07  SUBJETRES-KANANAME          PIC X(100).
      *           ������
                  07  SUBJETRES-BIRTHDAY          PIC X(10).
      *           ����
                  07  SUBJETRES-SEX               PIC X(1).
      *        ����ǯ��
               05  SUBJETRES-PERFORM-DATE      PIC X(07).
      *        �ɾ��ꥹ��
               05  SUBJETRES-SUBJECTIVES-AREA.
                   07  SUBJETRES-SUBJECTIVES-INFO     OCCURS   50.
      *               ������ʬ
                      09  SUBJETRES-OUTPATIENT-CLASS    PIC X(01).
      *               ���Ų�
                      09  SUBJETRES-DEP-CODE            PIC X(02).
      *               ���Ų�û��̾��
                      09  SUBJETRES-DEP-NAME            PIC X(80).
      *               �ݸ��ȹ礻����
                      09  SUBJETRES-INSURANCE-INFO.
      *                   �ݸ��ȹ礻�ֹ�
                          11  SUBJETRES-COMBINATION-NUMBER  PIC X(04).
      *                   �ݸ��ȹ礻ͭ������
                          11  SUBJETRES-COMB-START-DATE       PIC X(10).
                          11  SUBJETRES-COMB-END-DATE         PIC X(10).
      *
      *                   �ݸ��μ���
                          11  SUBJETRES-MAIN-INSURANCE-CLASS  PIC X(03).
      *                   �ݸ��μ���̾��
                          11  SUBJETRES-MAIN-INSURANCE-NAME   PIC X(20).
      *                   �ݸ����ֹ�
                          11  SUBJETRES-MAIN-INSURANCE-NUMBER PIC X(08).
      *                   ����
                          11  SUBJETRES-MAIN-MARK             PIC X(80).
      *                   �ֹ�
                          11  SUBJETRES-MAIN-NUMBER           PIC X(80).
      *R03.01  ����
                          11  SUBJETRES-MAIN-EDABAN           PIC X(02).
      *                   �����ʬ
                          11  SUBJETRES-MAIN-ASSISTANCE       PIC X(01).
      *                   �����ʬ̾��
                          11  SUBJETRES-MAIN-ASSISTANCE-NAME  PIC X(30).
      *                   �ܿͲ�²��ʬ
                          11  SUBJETRES-MAIN-FAMILY           PIC X(01).
      *                   �������
                          11  SUBJETRES-PUBLIC-INSURANCE-INFO
                                                    OCCURS  4 TIMES.
      *                       ����μ���
                              13  SUBJETRES-INSURANCE-CLASS   PIC X(03).
      *                       ����μ���̾��
                              13  SUBJETRES-INSURANCE-NAME    PIC X(20).
      *                       ��ô���ֹ�
                              13  SUBJETRES-PROVIDER-NUMBER   PIC X(08).
      *                       ������ֹ�
                              13  SUBJETRES-RECIPIENT-NUMBER  PIC X(20).
      *                   �ݸ��ȹ礻����
                          11  SUBJETRES-COMBINATION-MODE      PIC X(01).
      *               ������
                      09  SUBJETRES-PERFORM-DAY            PIC X(02).
      *               �ɾ��ܵ���ʬ������
                      09  SUBJETRES-SUBJECTIVES-CODE       PIC X(02).
      *               �ɾ��ܵ���ʬ̾��
                      09  SUBJETRES-SUBJECTIVES-NAME       PIC X(50).
      *               Ϣ��
                      09  SUBJETRES-RENNUM                 PIC X(02).
      *        �ɾ��ܵ�
               05  SUBJETRES-DETAIL-AREA.
      *           �ɾ��ܵ�����
                  07  SUBJETRES-DETAIL-RECORD          PIC X(4800).
      *
      *
