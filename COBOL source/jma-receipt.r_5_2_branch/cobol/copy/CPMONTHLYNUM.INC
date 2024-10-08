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
      *    ����������ֹ�           �� COPY  (CPMONTHLYNUM.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01   NACL-ƣ��     03.01.20  ������ʬ���ɲ�
      *
      *  03.04.01   NACL-ƣ��     07.02.06  ����ɣĤ��ɲ�
      *  05.02.01    ORCAMO       24/01/15  �����޽���ʳ�ǧ�б�
      *                                     �����ֹ��ɲ�
      ************************************************************
      *01  MONTHLYNUM-REC.
      *����
           03  MONTHLYNUM-KEY.
      *���ŵ��أɣ�
               05  MONTHLYNUM-HOSPNUM         PIC 9(2).
      *���ԣɣ�
               05  MONTHLYNUM-PTID             PIC  9(10).
      *�ݸ��ֹ�ʸ����
               05  MONTHLYNUM-KOHNUM           PIC  X(03).
      *����ɣ�
               05  MONTHLYNUM-KOHID            PIC  9(10).
      *����ǯ��
               05  MONTHLYNUM-SRYYM            PIC  X(06).
      *������ʬ
               05  MONTHLYNUM-NYUGAIKBN        PIC  X(01).
      *������ֹ�
           03  MONTHLYNUM-JKYSNUM              PIC  X(20).
      *�����ֹ�
           03  MONTHLYNUM-KOFUNUM              PIC  X(15).
      *ü���ɣ�
           03  MONTHLYNUM-TERMID               PIC  X(32).
      *���ڡ��졼���ɣ�
           03  MONTHLYNUM-OPID                 PIC  X(16).
      *����ǯ����
           03  MONTHLYNUM-CREYMD               PIC  X(08).
      *����ǯ����
           03  MONTHLYNUM-UPYMD                PIC  X(08).
      *��������
           03  MONTHLYNUM-UPHMS                PIC  X(06).
      *-----> �����ѹ���
      *��ô���ֹ�
           03  MONTHLYNUM-XXFTNJANUM           PIC  X(08).
      *Ŭ�ѳ���ǯ����
           03  MONTHLYNUM-XXTEKSTYMD           PIC  X(08).
      *Ŭ�ѽ�λǯ����
           03  MONTHLYNUM-XXTEKEDYMD           PIC  X(08).
