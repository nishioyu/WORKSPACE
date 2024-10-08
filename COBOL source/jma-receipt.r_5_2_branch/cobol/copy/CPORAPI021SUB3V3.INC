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
      * 05.00.00     NACL-¿��Ǽ  20/03/XX  �ң���ǯ��������б�
      *****************************************************************
      ******************************************************************
      *    ���Уɺ޿��������Խ����֥ѥ�᥿�ʰ��β���
      *                                         (CPORAPI021SUB3V3.INC)
      ******************************************************************
       01  ORAPI021SUB3V3AREA.
      *    ������ʬ
           03  ORAPISUB3-SYORIKBN      PIC X(01).
      *
      *    �ơ��֥���
           03  ORAPISUB3-ZAI-MAX       PIC 9(04).
      *
      *SPA�ֵ�
           03  ORAPISUB3-HENSYU-AREA.
               05  ORAPISUB3-HENSYU-OCC        OCCURS   400.
      *        ���Ų������
                   07  ORAPISUB3-ZAINUM        PIC 9(08).
                   07  ORAPISUB3-SRYKA         PIC X(02).
                   07  ORAPISUB3-SRYKA-NAME    PIC X(20).
                   07  ORAPISUB3-HKNCOMBI      PIC X(04).
      *
      *            �ݸ��ȹ礻���֡�1:�����2:���ֳ���
                   07  ORAPISUB3-COMBINATION-MODE   PIC X(01).
      *
                   07  ORAPISUB3-SRYKBN        PIC X(02).
                   07  ORAPISUB3-SRYSYUKBN     PIC X(03).
      *            ������ʬ
                   07  ORAPISUB3-JIHOKBN       PIC X(01).
      *            ������
                   07  ORAPISUB3-ZAITEN        PIC 9(08).
      *            �ޥ��ʥ���ʬ
                   07  ORAPISUB3-PLUSKBN       PIC 9(01).
                   07  ORAPISUB3-ZAIKBN        PIC 9(03).
      *            ������
                   07  ORAPISUB3-JIHIMONEYTOTAL    PIC  9(07).
      *            �޲��
                   07  ORAPISUB3-NAI-DAY-AREA.
                       09  ORAPISUB3-NAI-DAY-TBL   OCCURS   10.
                           11   ORAPISUB3-NAI-DAY  PIC  9(03)
                                                       OCCURS   31.
      *            ����Ĵ������ư�����ʬ
                   07  ORAPISUB3-CHOUZAIAUTO   PIC X(01).
      *        API�ֵ�
      *            ������
                   07  ORAPISUB3-MDC-POINT         PIC X(08).
      *            �޶��
                   07  ORAPISUB3-MDC-MONEY         PIC X(08).
      *            �޶�ʬ�ʣ������ޡ�
                   07  ORAPISUB3-MDC-CODE          PIC X(01).
      *            ��縡���θ�����
                   07  ORAPISUB3-HKTKNSA-CNT       PIC X(02).
      *            ���ż���̾��
                   07  ORAPISUB3-MDC-CLASS-NAME    PIC X(40).
      *            �ݸ��ȹ礻̾��
                   07  ORAPISUB3-MAIN-INS-NAME     PIC X(20).
                   07  ORAPISUB3-PUBLIC-INS-INFO.
      *            ����μ���̾��
                       09  ORAPISUB3-INSURANCE-NAME  PIC X(20)
                                                     OCCURS  4 TIMES.
      *
      *            ��������
                   07  ORAPISUB3-SRYACT-AREA.
                       09  ORAPISUB3-SRYACT-TBL   OCCURS   50.
                           11   ORAPISUB3-SRYCD        PIC X(09).
                           11   ORAPISUB3-SRYSURYO     PIC 9(05)V9(05).
                           11   ORAPISUB3-SRYKAISU     PIC 9(03).
                           11   ORAPISUB3-MEISKYFLG    PIC X(01).
                           11   ORAPISUB3-AUTOKBN      PIC X(01).
                           11   ORAPISUB3-INPUTNUM     PIC 9(03).
      *                    ������
                           11   ORAPISUB3-JIHIMONEY    PIC 9(07).
      *                    ������������
                           11   ORAPISUB3-KANSURYO     PIC 9(05)V9(05).
      *
      *        API�ֵ�
      *                    ����̾��
                           11  ORAPISUB3-PRSCRPT-NAME      PIC X(140).
      *R02.6
      *                   �����ȡ�30�����ѣ���ʸ��
                           11  ORAPISUB3-PRSCRPT-NAME-INPUT  PIC X(200).
      *                    ����
                           11  ORAPISUB3-PRSCRPT-NUMBER    PIC X(11).
      *                    ñ�̥�����
                           11  ORAPISUB3-PRSCRPT-TANICD    PIC X(03).
      *                    ñ��̾��
                           11  ORAPISUB3-PRSCRPT-TANICD-NAME PIC X(24).
      *                    ��������������
                           11  ORAPISUB3-PRSCRPT-ATAI-G.
                               13  ORAPISUB3-PRSCRPT-ATAI  PIC X(10)
                                                         OCCURS   5.
      *                    ������
                           11  ORAPISUB3-PRSCRPT-MONEY     PIC X(07).
      *                    �ե����ʬ���
                           11  ORAPISUB3-PRSCRPT-FILMNUM   PIC X(03).
      *                    ��³�ؼ���ʬ
                           11  ORAPISUB3-PRSCRPT-CONTKBN   PIC X(01).
      *                    ��ư��ʬ���ֵ��ѡ�
                           11  ORAPISUB3-PRSCRPT-AUTOKBN   PIC X(01).
      *
      *    ���顼��ʬ
           03  ORAPISUB3-ERRCD         PIC X(04).
           03  ORAPISUB3-ERRMSG        PIC X(100).
      *
