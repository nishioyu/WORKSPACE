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
      *    ���Уɥ쥹�ݥ󥹿��Ź԰پ�������ʼ���ǯ��������ƣ���(xml2)
      *                                      COPY (CPMDCGET04V2RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  05.00.00    NMED-¿��Ǽ  21/01/XX  �ݸ������ɲ��б�
      *****************************************************************
        01  XML-MEDICALGET04RES.
            02  MDC04RES-CONTEXT           PIC S9(9)   BINARY.
            02  MDC04RES-OBJECT            PIC X(36).
            02  MDC04RES-MODE              PIC S9(9)   BINARY.
            02  MDC04RES-RECORDNAME        PIC X(128).
            02  MDC04RES-MEDICALRES.
      *       �»���
              03  MDC04RES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  MDC04RES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  MDC04RES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  MDC04RES-API-RESULT-MSG      PIC X(100).
      *
              03  MDC04RES-RESKEY              PIC X(100).
      *       ���Ծ���
              03  MDC04RES-PATIENT-INFOMATION.
      *           �����ֹ�
                  05  MDC04RES-PATIENTID         PIC X(20).
      *           ���Ի�̾
                  05  MDC04RES-NAME              PIC X(100).
      *           ���ԥ��ʻ�̾
                  05  MDC04RES-KANANAME          PIC X(100).
      *           ��ǯ����
                  05  MDC04RES-BIRTHDAY          PIC X(10).
      *           ����
                  05  MDC04RES-SEX               PIC X(1).
      *       ����ǯ��
              03  MDC04RES-PERFORM-DATE        PIC X(07).
      *       ���Ų�
              03  MDC04RES-DEP-CODE            PIC X(2).
      *       ���Ų�û��̾��
              03  MDC04RES-DEP-NAME            PIC X(20).
      *       �ݸ��ȹ礻����
              03  MDC04RES-COMBINATION-INF     OCCURS   5.
      *           �ݸ��ȹ礻�ֹ�
                  05  MDC04RES-COMBINATION-NUMBER  PIC X(04).
      *           �ݸ��ȹ礻����
                  05  MDC04RES-INSURANCE-INFO.
      *               �ݸ��μ���
                      07  MDC04RES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               �ݸ��μ���̾��
                      07  MDC04RES-MAIN-INSURANCE-NAME    PIC X(20).
      *               �ݸ����ֹ�
                      07  MDC04RES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *               ����
                      07  MDC04RES-MAIN-MARK              PIC X(80).
      *               �ֹ�
                      07  MDC04RES-MAIN-NUMBER            PIC X(80).
      *R03.01
      *               ����
                      07  MDC04RES-MAIN-EDABAN            PIC X(02).
      *               �������
                      07  MDC04RES-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *                   ����μ���
                          09  MDC04RES-INSURANCE-CLASS     PIC X(03).
      *                   ����μ���̾��
                          09  MDC04RES-INSURANCE-NAME      PIC X(20).
      *                   ��ô���ֹ�
                          09  MDC04RES-PROVIDER-NUMBER     PIC X(08).
      *                   ������ֹ�
                          09  MDC04RES-RECIPIENT-NUMBER    PIC X(20).
      *       �������ƾ���
              03  MDC04RES-MEDICAL-INF         OCCURS   31.
      *           ������
                  05  MDC04RES-MDC-DAY             PIC X(02).
                  05  MDC04RES-MEDICAL-INF2        OCCURS   40.
      *               ���Ŷ�ʬ
                      07  MDC04RES-MDC-CLASS           PIC X(02).
      *               ���ż��̶�ʬ
                      07  MDC04RES-MDC-CLASS2          PIC X(03).
      *               ���ż���̾��
                      07  MDC04RES-MDC-CLASS-NAME      PIC X(50).
      *               �޾���
                      07  MDC04RES-MEDICAL-INF3        OCCURS   100.
      *               ������
                          09  MDC04RES-MDC-POINT           PIC X(08).
      *               ��ۡʼ���ϫ�ұߡ�
                          09  MDC04RES-MDC-MOENY           PIC X(08).
      *               ���
                          09  MDC04RES-MDC-CLASS-NUMBER    PIC X(03).
      *               �޶�ʬ
                          09  MDC04RES-MDC-CODE            PIC X(01).
      *               ����ʬ��True�����ޡ�
                          09  MDC04RES-INCLUSION-CLASS     PIC X(05).
      *
