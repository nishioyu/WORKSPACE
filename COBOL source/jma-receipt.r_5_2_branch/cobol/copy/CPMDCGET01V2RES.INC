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
      *    ���Уɥ쥹�ݥ󥹿��Ź԰پ�������ʼ��������(xml2)
      *                                      COPY (CPMDCGET01V2RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.00    NACL-¿��Ǽ  15/12/14  ��ɼ�ֹ��ɲ��б�
      *  05.00.00    NMED-¿��Ǽ  21/01/XX  �ݸ������ɲ��б�
      *****************************************************************
        01  XML-MEDICALGET01RES.
            02  MDC01RES-CONTEXT           PIC S9(9)   BINARY.
            02  MDC01RES-OBJECT            PIC X(36).
            02  MDC01RES-MODE              PIC S9(9)   BINARY.
            02  MDC01RES-RECORDNAME        PIC X(128).
            02  MDC01RES-MEDICALRES.
      *       �»���
              03  MDC01RES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  MDC01RES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  MDC01RES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  MDC01RES-API-RESULT-MSG      PIC X(100).
      *
              03  MDC01RES-RESKEY              PIC X(100).
      *       ���Ծ���
              03  MDC01RES-PATIENT-INFOMATION.
      *           �����ֹ�
                  05  MDC01RES-PATIENTID         PIC X(20).
      *           ���Ի�̾
                  05  MDC01RES-NAME              PIC X(100).
      *           ���ԥ��ʻ�̾
                  05  MDC01RES-KANANAME          PIC X(100).
      *           ������
                  05  MDC01RES-BIRTHDAY          PIC X(10).
      *           ����
                  05  MDC01RES-SEX               PIC X(1).
      *       �����������
              03  MDC01RES-MEDICAL-INF     OCCURS   150.
      *           ��������
                  05  MDC01RES-PERFORM-DATE        PIC X(10).
      *           ���Ų�
                  05  MDC01RES-DEP-CODE            PIC X(02).
      *           ���Ų�û��̾��
                  05  MDC01RES-DEP-NAME            PIC X(20).
      *           Ϣ��
                  05  MDC01RES-NUMBER              PIC X(01).
      *           �ݸ��ȹ礻�ֹ�
                  05  MDC01RES-COMBINATION-NUMBER  PIC X(04).
      *           �ݸ��ȹ礻����
                  05  MDC01RES-INSURANCE-INFO.
      *             �ݸ��μ���
                    06  MDC01RES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             �ݸ��μ���̾��
                    06  MDC01RES-MAIN-INSURANCE-NAME    PIC X(20).
      *             �ݸ����ֹ�
                    06  MDC01RES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             ����
                    06  MDC01RES-MAIN-MARK              PIC X(80).
      *             �ֹ�
                    06  MDC01RES-MAIN-NUMBER            PIC X(80).
      *R03.01
      *             ����
                    06  MDC01RES-MAIN-EDABAN            PIC X(02).
      *             �������
                    06  MDC01RES-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *               ����μ���
                      07  MDC01RES-INSURANCE-CLASS         PIC X(03).
      *               ����μ���̾��
                      07  MDC01RES-INSURANCE-NAME          PIC X(20).
      *               ��ô���ֹ�
                      07  MDC01RES-PROVIDER-NUMBER         PIC X(08).
      *               ������ֹ�
                      07  MDC01RES-RECIPIENT-NUMBER        PIC X(20).
      *           ��ɼ�ֹ�
                  05  MDC01RES-INVOICD-NUMBER    PIC X(07).
      *R06.7      ��������
      *           ��Ͽ����
                  05  MDC01RES-CREYMD             PIC X(10).
      *           ��������
                  05  MDC01RES-UPYMD              PIC X(10).
      *           ��������
                  05  MDC01RES-UPTIME             PIC X(08).
      *
