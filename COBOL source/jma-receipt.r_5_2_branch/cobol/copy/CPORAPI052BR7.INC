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
      *    ���Уɥ쥹�ݥ󥹿��Ź԰پ��������
      *                                      COPY (CPORAPI052BR7.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  05.01.00    NMED-¿��Ǽ  20/11/XX  �������������б�
      *****************************************************************
      *01  MDCRES-OUT-REC.
      *    ���Ծ���
           03  MDCRES-PATIENT-INFOMATION.
      *        �����ֹ�
               05  MDCRES-PATIENTID         PIC X(20).
      *        ���Ի�̾
               05  MDCRES-NAME              PIC X(100).
      *        ���ԥ��ʻ�̾
               05  MDCRES-KANANAME          PIC X(100).
      *        ������
               05  MDCRES-BIRTHDAY          PIC X(10).
      *        ����
               05  MDCRES-SEX               PIC X(1).
      *    ��������
           03  MDCRES-PERFORM-DATE        PIC X(10).
      *    ���Ų�
           03  MDCRES-DEP-CODE            PIC X(2).
      *    ���Ų�û��̾��
           03  MDCRES-DEP-NAME            PIC X(20).
      *    Ϣ��
           03  MDCRES-RENNUM              PIC X(01).
      *    ��ɼ�ֹ�ʳ���Τߡ�
           03  MDCRES-INVOICD-NUMBER      PIC X(07).
      *    �ݸ��ȹ礻�ֹ�
           03  MDCRES-COMBINATION-NUMBER  PIC X(04).
      *    �ɥ������ʳ���Τߡ�
           03  MDCRES-DRCD                PIC X(5).
      *    �ɥ�������̾
           03  MDCRES-DRCD-NAME           PIC X(80).
      *    �������աʼ��������
           03  MDCRES-UPD-DATE            PIC X(10).
      *    �������֡ʼ��������
           03  MDCRES-UPD-TIME            PIC X(8).
      *
           03  MDCRES-MEDICAL-INF-OVERFLOW
                                          PIC X(5).
      *    �������ƾ���
           03  MDCRES-MEDICAL-INF.
      *        ���Ź԰�����
               05  MDCRES-MDC-INFO        OCCURS   50.
      *            ���ż��̶�ʬ
                   07  MDCRES-MDC-CLASS           PIC X(03).
      *            ���ż��̶�ʬ̾��
                   07  MDCRES-MDC-CLASS-NAME      PIC X(50).
      *            ���
                   07  MDCRES-MDC-CLASS-NUMBER    PIC X(03).
      *            ����
                   07  MDCRES-MDC-POINT           PIC X(08).
      *            ��ۡʼ���ϫ�ұߡ�
                   07  MDCRES-MDC-MOENY           PIC X(08).
      *            �޶�ʬ
                   07  MDCRES-MDC-CODE            PIC X(01).
      *            ����ʬ��True�����ޡ�
                   07  MDCRES-INCLUSION-CLASS     PIC X(05).
      *            ��縡���θ�����
                   07  MDCRES-HKTKNSA-CNT         PIC X(02).
      *            ������
                   07  MDCRES-PRESCRIPTION-INFO   OCCURS  50.
      *                ���ť�����
                       09  MDCRES-PRSCRPT-CODE         PIC X(9).
      *                ����̾�Ρʥ����ȡ�
                       09  MDCRES-PRSCRPT-NAME         PIC X(140).
      *R02.7           ����̾�Ρ����ϥ����ȡ�
                       09  MDCRES-PRSCRPT-NAME-INPUT   PIC X(200).
      *                ����
                       09  MDCRES-PRSCRPT-NUMBER       PIC X(11).
      *                ñ��
                       09  MDCRES-UNIT-CODE            PIC X(03).
      *                ñ��̾��
                       09  MDCRES-UNIT-NAME            PIC X(24).
      *R02.11
      *                ��������������
                       09  MDCRES-PRSCRPT-ATAI-G.
                           11  MDCRES-PRSCRPT-ATAI     PIC X(10)
                                                       OCCURS   5.
      *
