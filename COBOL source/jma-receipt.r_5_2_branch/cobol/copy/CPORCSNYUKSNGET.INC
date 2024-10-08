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
      *    �������û������ѥ�᥿����COPY (CPORCSNYUKSNGET.INC)
      *                                ���ѥץ������ (ORCSNYUKSNGET)
      *    �������
      *      ���ŵ��أɣ�
      *      ���ԣɣ�
      *      ���곫����
      *      ������������
      *    �ֵѹ���
      *      �������û���ʬ
      *      �������û�������
      *      ����������Ƚ��ե饰
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSNYUKSNGET-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-NYUKSNGET-HOSPNUM                 PIC  9(02).
      *���ԣɣ�
           03  LNK-NYUKSNGET-PTID                    PIC  9(10).
      *���곫����
           03  LNK-NYUKSNGET-SANTEIYM                PIC  X(06).
      *���������ֹ�
           03  LNK-NYUKSNGET-BTUNUM                  PIC  X(02).
      *�����¼��ֹ�
           03  LNK-NYUKSNGET-BRMNUM                  PIC  X(08).
      *������������
           03  LNK-NYUKSNGET-NYUIN-SRYCD             PIC  X(09).
      *���������������׷ﳰ���������������������ɤ�������������û�
      *������Ƚ�ꤹ�����Ƚ���ʬ
           03  LNK-NYUKSNGET-GAI-NYUKSNKBN           PIC  X(01).
      *���ౡ��Ͽ��������βû��ΰ�
           03  LNK-NYUKSNGET-I01-KSN-SRYCD           PIC  X(09)
                                                     OCCURS  3.
           03  LNK-NYUKSNGET-FLG-IPPAN               PIC  9(01).
           03  LNK-NYUKSNGET-FLG-IPPAN2              PIC  9(01).
      *�ֵѹ���
           03  LNK-NYUKSNGET-HENKYAKU-AREA.
      *�������û��������ֵ��ΰ�
             05  LNK-NYUKSNGET-HENKYAKU-OCC          OCCURS  30.
      *�������û���ʬ
                 07  LNK-NYUKSNGET-NYUKSNKBN         PIC  X(01).
      *�������û�������
                 07  LNK-NYUKSNGET-KSNSRYCD          PIC  X(09).
      *������������ʬ
           03  LNK-NYUKSNGET-TOKUBETSU-NYUIN         PIC  X(01).
      *���������ɡʣ����������û��ֵѤ��ꡡ�����������û��ֵѤʤ���
           03  LNK-NYUKSNGET-RC                      PIC S9(9) BINARY.
      *
