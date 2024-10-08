      ******************************************************************
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
      ******************************************************************
      *    ��ϥӥ곫�����������Խ�����   COPY (CPORCSRIHASTDAY.INC)
      ******************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ******************************************************************
       01  ORCSRIHASTDAYAREA.
      *
      *���ŵ��أɣ�
           03  LNK-RIHASTDAY-HOSPNUM              PIC  9(02).
      *������ʬ
           03  LNK-RIHASTDAY-NYUGAIKBN            PIC  X(01).
      *���ԣɣ�
           03  LNK-RIHASTDAY-PTID                 PIC  9(10).
      *���Ų�
           03  LNK-RIHASTDAY-SRYKA                PIC  X(02).
      *����ǯ��
           03  LNK-RIHASTDAY-SRYYM                PIC  X(06).
      *�ݸ��ȹ礻
           03  LNK-RIHASTDAY-HKNCOMBI             PIC  9(04).
      *
      *�ֵѹ���
           03  LNK-RIHASTDAY-OUT-AREA.
              04  LNK-RIHASTDAY-OUT-DAT.
      *           ���������ɼ���
      *           ���������ɼ���������������ϲû�
      *           ����Ǿ��ɼ�����
      *           �������Ѿɸ���
      *           �������Ѿɸ���������������ϲû�
      *           ������ư��
      *           �����Ƶ۴�
      *           �����Ƶ۴������������ϲû�
      *           �����ݿ�
      *         ���������´���
      *         �������㳲���ʼԡ�
                 05  LNK-RIHASTDAY-OUT-OCC          OCCURS  11.
                   06  LNK-RIHASTDAY-OUT-OCC2       OCCURS 100.
                      07  LNK-RIHASTDAY-MARK        PIC  X(01).
                      07  LNK-RIHASTDAY-YUKOKETA    PIC  9(03).
                      07  LNK-RIHASTDAY-COM         PIC  X(200).
      *            ����������Ϥ����ä���ϥӥ���Ź԰٥�����
                   06  LNK-RIHASTDAY-OUT-OCC3.
                      07  LNK-RIHASTDAY-SRYCD       PIC  X(09)
                                                  OCCURS  50.
      *            ��ϥӥ곫���������Ȥ�ͭ������ǥå���
                   06  LNK-RIHASTDAY-IDX            PIC  9(03).
      *       ��ϥӥꥷ���ƥ�ͽ�󥳡��ɰ��� (LNK-RIHASTDAY-HOSPNUM = ZERO �λ��ֵ�)
              04  LNK-RIHASTDAY-SYSCDLST          PIC  X(09)
                                                  OCCURS  50.
              04  LNK-RIHASTDAY-SYSCDLST-IDX      PIC  9(03).
      *       ��ϥӥꥳ���ɰ��� (LNK-RIHASTDAY-HOSPNUM = ZERO �λ��ֵ�)
              04  LNK-RIHASTDAY-SRYCDLST          PIC  X(09)
                                                  OCCURS  1000.
              04  LNK-RIHASTDAY-SRYCDLST-IDX      PIC  9(04).
      *
      *����������
           03  LNK-RIHASTDAY-RC                   PIC  9(02).
      *
