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
      *    ���Уɥ쥹�ݥ����ӥǡ��������������(xml2)
      *                                      COPY (CPTMDCGETV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *
      *****************************************************************
        01  XML-TMEDICALGETRES.
            02  TMDCRES-CONTEXT           PIC S9(9)   BINARY.
            02  TMDCRES-OBJECT            PIC X(36).
            02  TMDCRES-MODE              PIC S9(9)   BINARY.
            02  TMDCRES-RECORDNAME        PIC X(128).
            02  TMDCRES-MEDICALRES.
      *       �»���
              03  TMDCRES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  TMDCRES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  TMDCRES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  TMDCRES-API-RESULT-MSG      PIC X(100).
      *
              03  TMDCRES-RESKEY              PIC X(100).
      *
      *       ��������
              03  TMDCRES-PERFORM-DATE        PIC X(10).
      *       ������ʬ
              03  TMDCRES-OUTPATIENT-CLASS    PIC X(1).
      *
      *       ���ӥǡ�������
              03  TMDCRES-MEDICAL-INF         OCCURS   500.
      *           ���Ծ���
                  05  TMDCRES-PATIENT-INFOMATION.
      *               �����ֹ�
                      07  TMDCRES-PATIENTID        PIC X(20).
      *               ���Ի�̾
                      07  TMDCRES-NAME             PIC X(100).
      *               ���ԥ��ʻ�̾
                      07  TMDCRES-KANANAME         PIC X(100).
      *               ������
                      07  TMDCRES-BIRTHDAY         PIC X(10).
      *               ����
                      07  TMDCRES-SEX              PIC X(1).
      *           ���Ų�
                  05  TMDCRES-DEP-CODE             PIC X(02).
      *           ���Ų�û��̾��
                  05  TMDCRES-DEP-NAME             PIC X(20).
      *           �ɥ�����������
                  05  TMDCRES-DR-CODE              PIC X(5).
      *           �ɥ�������̾
                  05  TMDCRES-DR-NAME              PIC X(80).
      *           �ݸ��ȹ礻�ֹ�
                  05  TMDCRES-COMBINATION-NUMBER   PIC X(04).
      *           �ݸ��ȹ礻����
                  05  TMDCRES-INSURANCE-INFO.
      *             �ݸ��μ���
                    07  TMDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             �ݸ��μ���̾��
                    07  TMDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             �ݸ����ֹ�
                    07  TMDCRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             ����
                    07  TMDCRES-MAIN-MARK              PIC X(80).
      *             �ֹ�
                    07  TMDCRES-MAIN-NUMBER            PIC X(80).
      *R03.01
      *             ����
                    07  TMDCRES-MAIN-EDABAN            PIC X(02).
      *             �������
                    07  TMDCRES-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *               ����μ���
                      09  TMDCRES-INSURANCE-CLASS         PIC X(03).
      *               ����μ���̾��
                      09  TMDCRES-INSURANCE-NAME          PIC X(20).
      *               ��ô���ֹ�
                      09  TMDCRES-PROVIDER-NUMBER         PIC X(08).
      *               ������ֹ�
                      09  TMDCRES-RECIPIENT-NUMBER        PIC X(20).
      *           Ʊ��������
                  05  TMDCRES-DOUJITSUKBN           PIC X(01).
      *
      *           �գɣ�
                  05  TMDCRES-MEDICAL-UID          PIC X(36).
      *           ��������
                  05  TMDCRES-UPHMS                PIC X(08).
      *           ���ֶ�ʬ����1:Ÿ�����
                  05  TMDCRES-MODE-FLG             PIC X(01).
      *           ���ֶ�ʬ����1:���ӽ�λ��Ͽ��
                  05  TMDCRES-KARTE-FLG            PIC X(01).
      *
