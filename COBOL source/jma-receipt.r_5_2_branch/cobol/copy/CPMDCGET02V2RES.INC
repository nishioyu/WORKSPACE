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
      *    ���Уɥ쥹�ݥ󥹿��Ź԰پ�������ʼ��������Ź԰����ơ�(xml2)
      *                                      COPY (CPMDCGET02V2RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 05.00.00     NACL-¿��Ǽ  17/08/XX  ��ɼ�ֹ��ɲ��б�
      * 05.00.00     NACL-¿��Ǽ  17/11/XX  �����ʤ��ɲ��б�
      * 05.00.00     NACL-¿��Ǽ  20/11/XX  ����������CLAIM���б�
      * 05.00.00     NACL-¿��Ǽ  20/11/XX  �������������ɲ��б�
      * 05.00.00     NMED-¿��Ǽ  21/01/XX  �ݸ������ɲ��б�
      *****************************************************************
        01  XML-MEDICALGET02RES.
            02  MDC02RES-CONTEXT           PIC S9(9)   BINARY.
            02  MDC02RES-OBJECT            PIC X(36).
            02  MDC02RES-MODE              PIC S9(9)   BINARY.
            02  MDC02RES-RECORDNAME        PIC X(128).
            02  MDC02RES-MEDICALRES.
      *       �»���
              03  MDC02RES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  MDC02RES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  MDC02RES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  MDC02RES-API-RESULT-MSG      PIC X(100).
      *
              03  MDC02RES-RESKEY              PIC X(100).
      *       ���Ծ���
              03  MDC02RES-PATIENT-INFOMATION.
      *           �����ֹ�
                  05  MDC02RES-PATIENTID         PIC X(20).
      *           ���Ի�̾
                  05  MDC02RES-NAME              PIC X(100).
      *           ���ԥ��ʻ�̾
                  05  MDC02RES-KANANAME          PIC X(100).
      *           ������
                  05  MDC02RES-BIRTHDAY          PIC X(10).
      *           ����
                  05  MDC02RES-SEX               PIC X(1).
      *       ��������
              03  MDC02RES-PERFORM-DATE        PIC X(10).
      *       ���Ų�
              03  MDC02RES-DEP-CODE            PIC X(2).
      *       ���Ų�û��̾��
              03  MDC02RES-DEP-NAME            PIC X(20).
      *       Ϣ��
              03  MDC02RES-NUMBER              PIC X(01).
      *       �������ƾ���
              03  MDC02RES-MEDICAL-INF     OCCURS   5.
      *           �ݸ��ȹ礻�ֹ�
                  05  MDC02RES-COMBINATION-NUMBER  PIC X(04).
      *           �ݸ��ȹ礻����
                  05  MDC02RES-INSURANCE-INFO.
      *               �ݸ��μ���
                      07  MDC02RES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               �ݸ��μ���̾��
                      07  MDC02RES-MAIN-INSURANCE-NAME    PIC X(20).
      *               �ݸ����ֹ�
                      07  MDC02RES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *               ����
                      07  MDC02RES-MAIN-MARK              PIC X(80).
      *               �ֹ�
                      07  MDC02RES-MAIN-NUMBER            PIC X(80).
      *R03.01
      *               ����
                      07  MDC02RES-MAIN-EDABAN            PIC X(02).
      *               �������
                      07  MDC02RES-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *                   ����μ���
                          09  MDC02RES-INSURANCE-CLASS     PIC X(03).
      *                   ����μ���̾��
                          09  MDC02RES-INSURANCE-NAME      PIC X(20).
      *                   ��ô���ֹ�
                          09  MDC02RES-PROVIDER-NUMBER     PIC X(08).
      *                   ������ֹ�
                          09  MDC02RES-RECIPIENT-NUMBER    PIC X(20).
      *           ��ɼ�ֹ�
                  05  MDC02RES-INVOICD-NUMBER    PIC X(07).
      *           ���Ź԰�����
                  05  MDC02RES-MDC-INFO        OCCURS   135.
      *               ���ż��̶�ʬ
                      07  MDC02RES-MDC-CLASS           PIC X(03).
      *               ���ż��̶�ʬ̾��
                      07  MDC02RES-MDC-CLASS-NAME      PIC X(50).
      *               ���
                      07  MDC02RES-MDC-CLASS-NUMBER    PIC X(03).
      *               ����
                      07  MDC02RES-MDC-POINT           PIC X(08).
      *               ��ۡʼ���ϫ�ұߡ�
                      07  MDC02RES-MDC-MOENY           PIC X(08).
      *               �޶�ʬ
                      07  MDC02RES-MDC-CODE            PIC X(01).
      *               ����ʬ��True�����ޡ�
                      07  MDC02RES-INCLUSION-CLASS     PIC X(05).
      *H29.11
      *               ��縡���θ�����
                      07  MDC02RES-HKTKNSA-CNT         PIC X(02).
      *               ������
                      07  MDC02RES-PRESCRIPTION-INFO   OCCURS  50.
      *                   ���ť�����
                          09  MDC02RES-PRSCRPT-CODE         PIC X(9).
      *                   ����̾�Ρʥ����ȡ�
                          09  MDC02RES-PRSCRPT-NAME         PIC X(140).
      *RO20.6             ���ϥ���������
                          09  MDC02RES-PRSCRPT-NAME-INPUT   PIC X(200).
      *                   ����
                          09  MDC02RES-PRSCRPT-NUMBER       PIC X(11).
      *                   ñ��
                          09  MDC02RES-UNIT-CODE            PIC X(03).
      *                   ñ��̾��
                          09  MDC02RES-UNIT-NAME            PIC X(24).
      *R02.11
      *                   ��������������
                          09  MDC02RES-PRSCRPT-ATAI-G.
                              11  MDC02RES-PRSCRPT-ATAI     PIC X(10)
                                                            OCCURS   5.
      *H29.11
      *                   �����ޥ�������������
                          09  MDC02RES-TENSIKIBETU          PIC X(01).
      *                   �����ޥ���������
                          09  MDC02RES-TEN                  PIC X(10).
      *R02.11
      *                   ���������ʥޥ����������˿��̡������ȿ�ǡ�
                          09  MDC02RES-REFER-TEN            PIC X(10).
      *
