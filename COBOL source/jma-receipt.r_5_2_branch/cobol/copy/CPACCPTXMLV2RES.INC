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
      *    ���Уɥ쥹�ݥ󥹼��վ���           COPY (CPACCPTXMLV2RES.INC)
      ******************************************************************
        01  XML-ACCEPTRES.
            02  ACCPTRES-CONTEXT          PIC S9(9)   BINARY.
            02  ACCPTRES-OBJECT           PIC X(36).
            02  ACCPTRES-MODE             PIC S9(9)   BINARY.
            02  ACCPTRES-RECORDNAME       PIC X(128).
            02  ACCPTRES-REGISTRES.
      *       �»���
              03  ACCPTRES-INFORMATION-DATE           PIC X(10).
      *       �»ܻ���
              03  ACCPTRES-INFORMATION-TIME           PIC X(8).
      *       �ֵѥ�����
              03  ACCPTRES-API-RESULT                 PIC X(2).
      *       �ֵѥ�å�����
              03  ACCPTRES-API-RESULT-MSG             PIC X(100).
      *       ��˥󥰥�å�����
              03  ACCPTRES-API-WARNING-MSG-G.
                  05  ACCPTRES-API-WARNING-MSG-TBL    OCCURS  5.
                      07  ACCPTRES-API-WARNING-MSG    PIC X(100).
      *
              03  ACCPTRES-RESKEY                     PIC X(100).
      *       ������
              03  ACCPTRES-REGIST-DATE                PIC X(10).
      *       ���ջ���
              03  ACCPTRES-REGIST-TIME                PIC X(8).
      *       ���գɣ�
              03  ACCPTRES-REGIST-ID                  PIC X(5).
      *       ���Ų�
              03  ACCPTRES-REGIST-DEP-CODE            PIC X(2).
      *       ���Ų�̾��
              03  ACCPTRES-REGIST-DEP-NAME            PIC X(80).
      *       �ɥ�����������
              03  ACCPTRES-REGIST-DR-CODE             PIC X(5).
      *       �ɥ�������̾
              03  ACCPTRES-REGIST-DR-NAME             PIC X(80).
      *       �������ƶ�ʬ
              03  ACCPTRES-REGIST-DEP-INFO            PIC X(2).
      *
              03  ACCPTRES-PATIENT-INFOMATION.
      *         �����ֹ�
                04  ACCPTRES-PATIENTID                PIC X(20).
      *         ���Ի�̾
                04  ACCPTRES-NAME                     PIC X(100).
      *         ���ԥ��ʻ�̾
                04  ACCPTRES-KANANAME                 PIC X(100).
      *         ��ǯ����
                04  ACCPTRES-BIRTHDAY                 PIC X(10).
      *         ����
                04  ACCPTRES-SEX                      PIC X(1).
      *         �������
                04  ACCPTRES-ADDRESS-INFO.
      *           ͹���ֹ�
                  05  ACCPTRES-HOME-ZIP-CODE          PIC X(7).
      *           ����
                  05  ACCPTRES-HOME-ADDRESSES         PIC X(100).
      *         �ݸ�����
                04  ACCPTRES-INSURANCE-G.
                  05  ACCPTRES-INSURANCE-INFO          OCCURS 30 TIMES.
      *             �ݸ��ȹ礻�ֹ�
                    06  ACCPTRES-COMBINATION-NUMBER     PIC X(04).
      *H29.04
      *             ��ɽ����ʬ
                    06  ACCPTRES-COMBINATION-HYOJI      PIC X(01).
      *
      *             �ݸ��μ���
                    06  ACCPTRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             �ݸ����ֹ�
                    06  ACCPTRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             �ݸ��μ���̾��
                    06  ACCPTRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             ����
                    06  ACCPTRES-MAIN-MARK              PIC X(80).
      *             �ֹ�
                    06  ACCPTRES-MAIN-NUMBER            PIC X(80).
      *R02.12       ����
                    06  ACCPTRES-MAIN-EDABAN            PIC X(02).
      *             ��³��ʬ
                    06  ACCPTRES-MAIN-CONTINUATION      PIC X(01).
      *             �����ʬ
                    06  ACCPTRES-MAIN-ASSISTANCE        PIC X(01).
      *             �ܿͲ�²��ʬ
                    06  ACCPTRES-MAIN-FAMILY            PIC X(01).
      *             ���ݸ���̾
                    06  ACCPTRES-MAIN-POLICYHOLDER      PIC X(100).
      *             ͭ��������
                    06  ACCPTRES-MAIN-START-DATE        PIC X(10).
      *             ͭ����λ��
                    06  ACCPTRES-MAIN-END-DATE          PIC X(10).
      *             �������
                    06  ACCPTRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               ����μ���
                      07  ACCPTRES-INSURANCE-CLASS         PIC X(03).
      *               ����μ���̾��
                      07  ACCPTRES-INSURANCE-NAME          PIC X(20).
      *               ��ô���ֹ�
                      07  ACCPTRES-PROVIDER-NUMBER         PIC X(08).
      *               ������ֹ�
                      07  ACCPTRES-RECIPIENT-NUMBER        PIC X(20).
      *               ��������ôΨ�ʳ��
                      07  ACCPTRES-RATE-ADMISSION          PIC X(04).
      *               �����ݸ����
                      07  ACCPTRES-MONEY-ADMISSION         PIC X(06).
      *               �������ôΨ�ʳ��
                      07  ACCPTRES-RATE-OUTPATIENT         PIC X(04).
      *               ����ݸ����
                      07  ACCPTRES-MONEY-OUTPATIENT        PIC X(06).
      *               ͭ��������
                      07  ACCPTRES-INSURANCE-START-DATE    PIC X(10).
      *               ͭ����λ��
                      07  ACCPTRES-INSURANCE-END-DATE      PIC X(10).
      *
