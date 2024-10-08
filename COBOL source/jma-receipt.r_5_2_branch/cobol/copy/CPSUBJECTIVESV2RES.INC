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
      *                                    COPY (CPSUBJECTIVEV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
        01  XML-SUBJECTIVESRES.
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
              05  SUBJETRES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              05  SUBJETRES-API-RESULT-MSG      PIC X(100).
      *       �ֵѥ�˥󥰥�å�����
              05  SUBJETRES-API-WARNING-MSG-G.
                  07  SUBJETRES-API-WARNING-MSG      PIC X(100)
                                                OCCURS   5.
      *
              05  SUBJETRES-RESKEY              PIC X(100).
      *       ��Ͽ����
              05  SUBJETRES-PATIENT-INFOMATION.
      *           ������ʬ
                  07  SUBJETRES-OUTPATIENT-CLASS  PIC X(01).
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
      *           ����ǯ��
                  07  SUBJETRES-PERFORM-DATE      PIC X(10).
      *           ���Ų�
                  07  SUBJETRES-DEP-CODE            PIC X(02).
      *           ���Ų�û��̾��
                  07  SUBJETRES-DEP-NAME            PIC X(80).
      *           �ݸ��ȹ礻�ֹ�
                  07  SUBJETRES-COMBINATION-NUMBER  PIC X(04).
      *           �ݸ��ȹ礻����
                  07  SUBJETRES-INSURANCE-INFO.
      *             �ݸ��μ���
                    09  SUBJETRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             �ݸ��μ���̾��
                    09  SUBJETRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             �ݸ����ֹ�
                    09  SUBJETRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             ����
                    09  SUBJETRES-MAIN-MARK              PIC X(80).
      *             �ֹ�
                    09  SUBJETRES-MAIN-NUMBER            PIC X(80).
      *R03.01  ����
                    09  SUBJETRES-MAIN-EDABAN            PIC X(02).
      *             ��³��ʬ
                    09  SUBJETRES-MAIN-CONTINUATION      PIC X(01).
      *             �����ʬ
                    09  SUBJETRES-MAIN-ASSISTANCE        PIC X(01).
      *             �����ʬ̾��
                    09  SUBJETRES-MAIN-ASSISTANCE-NAME   PIC X(30).
      *             �ܿͲ�²��ʬ
                    09  SUBJETRES-MAIN-FAMILY            PIC X(01).
      *             ���ݸ���̾
                    09  SUBJETRES-MAIN-POLICYHOLDER      PIC X(100).
      *             ͭ��������
                    09  SUBJETRES-MAIN-START-DATE        PIC X(10).
      *             ͭ����λ��
                    09  SUBJETRES-MAIN-END-DATE          PIC X(10).
      *             �������
                    09  SUBJETRES-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *               ����μ���
                      11  SUBJETRES-INSURANCE-CLASS         PIC X(03).
      *               ����μ���̾��
                      11  SUBJETRES-INSURANCE-NAME          PIC X(20).
      *               ��ô���ֹ�
                      11  SUBJETRES-PROVIDER-NUMBER         PIC X(08).
      *               ������ֹ�
                      11  SUBJETRES-RECIPIENT-NUMBER        PIC X(20).
      *               ��������ôΨ�ʳ��
                      11  SUBJETRES-RATE-ADMISSION          PIC X(04).
      *               �����ݸ����
                      11  SUBJETRES-MONEY-ADMISSION         PIC X(06).
      *               �������ôΨ�ʳ��
                      11  SUBJETRES-RATE-OUTPATIENT         PIC X(04).
      *               ����ݸ����
                      11  SUBJETRES-MONEY-OUTPATIENT        PIC X(06).
      *               ͭ��������
                      11  SUBJETRES-INSURANCE-START-DATE    PIC X(10).
      *               ͭ����λ��
                      11  SUBJETRES-INSURANCE-END-DATE      PIC X(10).
      *
      *           Ϣ��
                  07  SUBJETRES-NUMBER                 PIC X(02).
      *           �ɾ��ܵ���ʬ������
                  07  SUBJETRES-SUBJECTIVES-CODE       PIC X(02).
      *           �ɾ��ܵ���ʬ̾��
                  07  SUBJETRES-SUBJECTIVES-NAME       PIC X(50).
      *           �ɾ��ܵ�
                  07  SUBJETRES-DETAIL-RECORD          PIC X(4800).
      *
      *
