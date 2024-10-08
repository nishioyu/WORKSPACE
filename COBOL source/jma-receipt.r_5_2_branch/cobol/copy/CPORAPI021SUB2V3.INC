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
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 05.00.00     NACL-¿��Ǽ  19/01/XX  ʣ�����ݸ��б�
      *****************************************************************
      ******************************************************************
      *    ���Уɿ��������Խ����֥ѥ�᥿�ʰ��β���
      *                                         (CPORAPI021SUB2V3.INC)
      ******************************************************************
       01  ORAPI021SUB2V3AREA.
      *    ������ʬ
           03  ORAPISUB2-SYORIKBN      PIC X(01).
      *    �ơ��֥���
           03  ORAPISUB2-WKSRYACT-MAX  PIC 9(04).
      *
      *�ֵ�
           03  ORAPISUB2-HENSYU-AREA.
      *        ���������ֵ�
               05  ORAPISUB2-MEDICAL-INF-G.
                 07  ORAPISUB2-MDC-INFO          OCCURS   120.
      *!!!!
      *H31.01
      *             �ݸ������Ų�����
                    09  ORAPISUB2-CHENGE-REC.
      *                �ǡ�����ʬ��DEP��ʣ�����Ųʡ�COM:�ݸ��ȹ礻�ֹ�Υ쥳���ɡ�
                        11  ORAPISUB2-MDC-DATA-KBN         PIC X(03).
      *                 �ݸ��ȹ礻�ֹ�
                        11  ORAPISUB2-MDC-COMBINATION      PIC X(04).
      *                 �ݸ��μ���̾��
                        11  ORAPISUB2-MDC-MAIN-INS-NAME
                                                       PIC X(20).
                        11  ORAPISUB2-PUBLIC-INSURANCE-INFO.
      *                     ����μ���̾��
                            13  ORAPISUB2-INSURANCE-NAME  PIC X(20)
                                                       OCCURS  4 TIMES.
      *                 ���Ų�
                        11  ORAPISUB2-MDC-DEP-CODE         PIC X(02).
                        11  ORAPISUB2-MDC-DEP-CODE-NAME    PIC X(20).
      *                 �ɥ�����
                        11  ORAPISUB2-MDC-DR-CODE          PIC X(05).
                        11  ORAPISUB2-MDC-DR-CODE-NAME     PIC X(80).
      *!!!!
      *             ���Ŷ�ʬ
                    09  ORAPISUB2-MDC-CLASS-KBN         PIC X(2).
      *             ���ż��̶�ʬ
                    09  ORAPISUB2-MDC-CLASS             PIC X(3).
      *             ���ż���̾��
                    09  ORAPISUB2-MDC-CLASS-NAME        PIC X(40).
      *             ���
                    09  ORAPISUB2-MDC-CLASS-NUMBER      PIC X(03).
      *H29.11 ������
      *            ������
                    09  ORAPISUB02-MDC-DAY-G.
                       11  ORAPISUB02-MDC-DAY      PIC X(01)
                                                   OCCURS   31.
      *
      *             ������
                    09  ORAPISUB2-MDC-POINT             PIC X(08).
      *             �޶��
                    09  ORAPISUB2-MDC-MONEY             PIC X(08).
      *             �޶�ʬ�ʣ������ޡ�
                    09  ORAPISUB2-MDC-CODE              PIC X(01).
      *H29.5
      *             ��縡���θ�����
                    09  ORAPISUB2-HKTKNSA-CNT           PIC X(02).
      *
      *             �޺����ǽϢ��
                    09  ORAPISUB2-DEL-NUMBER            PIC X(02).
      *             �޺����ʬ
                    09  ORAPISUB2-DEL-INF               PIC X(01).
      *
                    09  ORAPISUB2-PRSCRPT-INFO-G.
                      10  ORAPISUB2-PRSCRPT-INFO    OCCURS   50.
      *                 ���ť�����
                        11  ORAPISUB2-PRSCRPT-CODE          PIC X(9).
      *                 ����̾��
                        11  ORAPISUB2-PRSCRPT-NAME          PIC X(140).
      *R02.6
      *                �����ȡ�30�����ѣ���ʸ��
                        11  ORAPISUB2-PRSCRPT-NAME-INPUT    PIC X(200).
      *                 ����
                        11  ORAPISUB2-PRSCRPT-NUMBER        PIC X(11).
      *H29.5
      *                 ñ�̥�����
                        11  ORAPISUB2-PRSCRPT-TANICD        PIC X(03).
      *                 ñ��̾��
                        11  ORAPISUB2-PRSCRPT-TANICD-NAME   PIC X(24).
      *                 ������
                        11  ORAPISUB2-PRSCRPT-MONEY         PIC X(07).
      *                 ��������������
                        11  ORAPISUB2-PRSCRPT-ATAI-G.
                            13  ORAPISUB2-PRSCRPT-ATAI      PIC X(10)
                                                         OCCURS   5.
      *H29.5
      *                 �ե����ʬ���
                        11  ORAPISUB2-PRSCRPT-FILMNUM       PIC X(03).
      *                 ����̾���ܶ�ʬ
                        11  ORAPISUB2-PRSCRPT-GENERIC       PIC X(01).
      *                 ����̾
                        11  ORAPISUB2-PRSCRPT-GENNAME       PIC X(200).
      *                 ��³�ؼ���ʬ
                        11  ORAPISUB2-PRSCRPT-CONTKBN       PIC X(01).
      *                 ����������ؼ���ʬ
                        11  ORAPISUB2-PRSCRPT-NAIFKBN       PIC X(01).
      *H29.5
      *                 ��ư����ʤ���ʬ�ʺ������ܼ��Ӳû���
                        11  ORAPISUB2-PRSCRPT-NOAUTO        PIC X(03).
      *                 ��ư��ʬ
                        11  ORAPISUB2-PRSCRPT-AUTOKBN       PIC X(01).
      *
      *        ������������(1:������2:�����
               05  ORAPISUB2-INGAIKBN      PIC X(01).
      *    ���顼��ʬ
           03  ORAPISUB2-ERRCD         PIC X(04).
           03  ORAPISUB2-ERRMSG        PIC X(100).
      *
