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
      *----------------------------------------------------------*
      *    ��ץ�����  ���������� ���̣ӣУ�
      *                             �ʣʣ������ʣ�����,�ʣ�������
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     MCC-¿��Ǽ   01.05.28  ����ɽ���򡢴�����
      * 01.00.02     MCC-¿��Ǽ   02.05.20  ϫ���ɲ�
      ************************************************************
      *
      *01  SPA-SCR-REC.
           03  SPA-GMN-TBL.
               05  SPA-GMN-TBLREC          OCCURS  400.
                   07  SPA-GMN-TBANGO      PIC 9(03).
                   07  SPA-GMN-TMEISYO     PIC X(80).
                   07  SPA-GMN-TSURYO.
                       09  SPA-GMN-TSURYOU      PIC X(11).
                       09  SPA-GMN-TTANI1       PIC X(01).
                       09  SPA-GMN-TTANMEI      PIC X(03).
      *R05.6       07  SPA-GMN-TTENSU      PIC X(07).
                   07  SPA-GMN-TTENSU      PIC X(08).
                   07  SPA-GMN-THKNCOMBI   PIC X(04).
                   07  SPA-GMN-TDAYGOK     PIC X(03).
                   07  SPA-GMN-TDAY-G.
                       09  SPA-GMN-TDAY    PIC X(03)
                                                   OCCURS  31.
                   07  SPA-GMN-TSRYCD      PIC X(09).
      *            ����̾��
                   07  SPA-GMN-GENENAME        PIC X(80).
      *
           03  SPA-NAI-G.
               05  SPA-NAI-TBL.
                   07  SPA-NAI-TBLREC  OCCURS   400.
                       09  SPA-NAI-TBANGO          PIC  9(04).
                       09  SPA-NAI-TBANGO2         PIC  9(03).
                       09  SPA-NAI-ENDBANGO        PIC  9(04).
                       09  SPA-NAI-SRYSYU          PIC  X(03).
                       09  SPA-NAI-SRYKA           PIC  X(02).
                       09  SPA-NAI-SRYKAMEI        PIC  X(02).
                       09  SPA-NAI-SRYKBN          PIC  X(02).
                       09  SPA-NAI-ZAINUM          PIC  9(08).
                       09  SPA-NAI-HKNCOMBI        PIC  9(04).
                       09  SPA-NAI-HKNCOMBIERR     PIC  9(01).
                       09  SPA-NAI-ROUJIN          PIC  9(01).
                       09  SPA-NAI-JIHOKBN         PIC  X(01).
                       09  SPA-NAI-ZAITEN          PIC  9(08).
                       09  SPA-NAI-SYRCDTOTAL      PIC  9(14).
                       09  SPA-NAI-SURYOTOTAL      PIC  9(07)V9(05).
                       09  SPA-NAI-MEISAISU        PIC  9(07).
                       09  SPA-NAI-ZAIKAISU        PIC  9(07).
                       09  SPA-NAI-SURYOU          PIC  9(05)V9(05).
                       09  SPA-NAI-SYUTEN1         PIC  9(07).
                       09  SPA-NAI-SYUTEN2         PIC  9(07).
                       09  SPA-NAI-ZAIKBN          PIC  9(03).
                       09  SPA-NAI-INPUTCHI-G.
                           11  SPA-NAI-INPUTCHI1       PIC  X(10).
                           11  SPA-NAI-INPUTCHI2       PIC  X(10).
                           11  SPA-NAI-INPUTCHI3       PIC  X(10).
                           11  SPA-NAI-INPUTCHI4       PIC  X(10).
                           11  SPA-NAI-INPUTCHI5       PIC  X(10).
                       09  SPA-NAI-INPUTCHI-R      REDEFINES
                                                   SPA-NAI-INPUTCHI-G.
                           11  SPA-NAI-INPUTCHI        PIC  X(10)
                                                       OCCURS  5.
                       09  SPA-NAI-DAY-AREA.
                           11  SPA-NAI-DAY-TBL     OCCURS   10.
                               13   SPA-NAI-DAY    PIC  9(03)
                                                       OCCURS   31.
                       09  SPA-NAI-RENNUM          PIC  9(02).
      *                Ϸ��Ŭ�Ѷ�ʬ
                       09  SPA-NAI-ROUTEKKBN       PIC  9(01).
      *                �������ޥ��ʥ�ʬ
                       09  SPA-NAI-ZAITENFLG       PIC  9(01).
      *                ���������ʬ
                       09  SPA-NAI-SANTEIRRKKBN    PIC  9(01).
      *                ����
                       09  SPA-NAI-JGNCNT          PIC  9(03).
      *                �����
                       09  SPA-NAI-JGNCNT1D        PIC  9(03).
      *                �����
                       09  SPA-NAI-JGNCNT1W        PIC  9(03).
      *                ����
                       09  SPA-NAI-JGNCNTETC       PIC  9(03).
      *                ���顼����
                       09  SPA-NAI-JGNCNTERR       PIC  9(01).
      *                ��ʬ����ٻȽ��
                       09  SPA-NAI-HOUKNSKBN       PIC  9(02).
      *                �����
                       09  SPA-NAI-TSURYO          PIC  9(05).
      *                ������ưȯ��Ĵ����
                       09  SPA-NAI-NYUAUTO         PIC  9(01).
      *ver.4.7         ������������
                       09  SPA-NAI-INGAITEN        PIC  9(07).
      *H28.5           ��ȯ���ѹ��Բġʽ����ˤ���
                       09  SPA-NAI-GENEFLG         PIC  X(01).
      *R03.7
      *                �����泫����
                       09  SPA-NAI-TNSSTR          PIC 9(02).
                    
      *
