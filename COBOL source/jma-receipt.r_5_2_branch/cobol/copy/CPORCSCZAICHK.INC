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
      ************************************************************
      *    ���Ź԰ٺ�̵ͭ�����å��ѥ�᥿����COPY (CPORCSCZAICHK.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  05.00.00    NACL-¿��Ǽ  20/06/XX  ���������������ɲ�
      ************************************************************
       01  ORCSCZAICHKAREA.
      *
      *��ʬ�ʣ��������å���
           03  ORCSCZAICHK-KBN                 PIC  X(01).
      *����
           03  ORCSCZAICHK-IN-AREA.
      *������
               05  ORCSCZAICHK-SRYYM               PIC  X(06).
      *���Ų�
               05  ORCSCZAICHK-SRYKA               PIC  X(02).
      *���Ŷ�ʬ
               05  ORCSCZAICHK-SRYKBN              PIC  X(02).
      *���ż���
               05  ORCSCZAICHK-SRYSYUKBN           PIC  X(03).
      *�ݸ��ȹ礻
               05  ORCSCZAICHK-HKNCOMBI            PIC  X(04).
      *������
               05  ORCSCZAICHK-ZAITEN              PIC  9(08).
      *���ť����ɷ�
               05  ORCSCZAICHK-SRYCDTOTAL          PIC  9(14).
      *���̷�
               05  ORCSCZAICHK-SURYOUTOTAL         PIC  9(07)V9(05).
      *���ٿ�
               05  ORCSCZAICHK-MEISAISU            PIC  9(07).
      *�޲��
               05  ORCSCZAICHK-ZAIKAISU            PIC  9(07).
      *���ݶ�ʬ�����ޡ�
               05  ORCSCZAICHK-JIHOKBN             PIC  X(01).
      *        ��������
               05  ORCSCZAICHK-MEISAI-TBL.
                   07  ORCSCZAICHK-MEISAI-OCC      OCCURS   50.
                       09  ORCSCZAICHK-SRYCD       PIC  X(09).
                       09  ORCSCZAICHK-SRYSURYO    PIC  9(05)V9(05).
                       09  ORCSCZAICHK-SRYKAISU    PIC  9(03).
                       09  ORCSCZAICHK-INPUTCOMENT PIC  X(140).
                       09  ORCSCZAICHK-INPUTCHI-G.
                           11  ORCSCZAICHK-ATAI    PIC  X(10)
                                                        OCCURS  5.
      *                �����Ϳ���
                       09  ORCSCZAICHK-KANSURYO    PIC  9(05)V9(05).
                       09  ORCSCZAICHK-INPUTKBN    PIC  X(01).
      *                ������
                       09  ORCSCZAICHK-JIHIMONEY   PIC  9(07).
      *R02.6           ��������������
                       09  ORCSCZAICHK-INPUTSTRING PIC  X(200).
      *����
           03  ORCSCZAICHK-OUT-AREA.
      *    �оݺ��ֹ�
               05  ORCSCZAICHK-ZAINUM              PIC  9(08).
      *
