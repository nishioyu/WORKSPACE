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
      *    �����쥻�ץȡ������ˡ�Ŭ�������پ���  COPY (CPRCF240.INC) 
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.04.01    NACL-����    10/03/29  �����������ɲ�
      *  04.05.01    NACL-����    10/01/19  �޶�ʬ�ɲ�
      *
      *  04.08.01    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *
      *  05.01.01    ORCAMO       22/06/08  ��쥻�ץȤε��ܽ��б�
      *                                     �ʿ��Ŷ�ʬ������������
      *  05.02.01    ORCAMO       24/04/26  ���£�ǯ�ٲ����б�
      *                                     CDKBN-KOUBAN�򣲷夫�飳����ѹ�
      ************************************************************
      *01  RECE240-REC.
           03  RECE240-KEY.
               05  RECE240-KEY1.
      *���ŵ����ֹ�
                   07  RECE240-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE240-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE240-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE240-PTID        PIC 9(10).
      *�쥻�ץȿ��Ų�
                   07  RECE240-RECEKA      PIC X(02).
      *����ɣ�
                   07  RECE240-HKNID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE240-TEKSTYMD    PIC X(08). 
      *���Ŷ�ʬ
               05  RECE240-SRYKBN          PIC X(02).
      *���ݶ�ʬ
               05  RECE240-JIHOKBN         PIC X(01).
      *�޶�ʬ
               05  RECE240-ZAIKBN          PIC 9(03).
      *�޼��̶�ʬ
               05  RECE240-ZAISKBKBN       PIC X(01).
      *�޼������ֹ�
               05  RECE240-ZAISKBBAN       PIC 9(08).
      *��ũ���϶�ʬ
               05  RECE240-TENTEKIKBN      PIC 9(02).
      *��ũ���϶�ʬ�����ֹ�
               05  RECE240-TENTEKIBAN      PIC 9(08).
      *�����»ܶ�ʬ
               05  RECE240-KNSJISKBN       PIC 9(01).
      *���ż��̶�ʬ
               05  RECE240-SRYSYUKBN       PIC X(03).
      *������
               05  RECE240-DAY             PIC 9(02).
      *ͥ���ʬ
               05  RECE240-PRIORITYKBN     PIC 9(01).
      *ɽ���ֹ�
               05  RECE240-CDKBN-SYO       PIC 9(01).
               05  RECE240-CDKBN-BU        PIC 9(02).
               05  RECE240-CDKBN-KBN       PIC X(01).
               05  RECE240-CDKBN-KBNNUM    PIC 9(03).
               05  RECE240-CDKBN-KBNNUM-EDA
                                           PIC 9(02).
               05  RECE240-CDKBN-KOUBAN    PIC 9(03).
               05  RECE240-KOUHYOJUNNUM    PIC 9(09).
      *��������
               05  RECE240-DAY2            PIC 9(02).
      *������
               05  RECE240-ZAITEN          PIC 9(08).
      *���ֹ�
               05  RECE240-ZAINUM          PIC 9(08).
      *Ϣ�ֹ�
               05  RECE240-LINENUM         PIC 9(05).
      *�����ٿ�
           03  RECE240-ZAIMEISAISU         PIC 9(03).
      *�����ֹ�
           03  RECE240-PTNUM               PIC X(20).
      *Ŭ�������
           03  RECE240-TEKIYO-INF.
      *�޶��ڡʡ���
               05  RECE240-ASTER           PIC X(02).
      *Ŧ��
               05  RECE240-TEKIYO          PIC X(100).
      *�ʥ��˥ե饰
           03  RECE240-SAFLG               PIC X(01).
