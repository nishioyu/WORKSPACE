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
      *    ϫ�ҥ쥻�ץ����ٽ��������     COPY (CPRCF087.INC)    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-����    05.05.25  ���ż��̶�ʬ�����������ɲ�
      *  01.00.02    NACL-����    05.07.28  �ޤ���Ƭ�ο��ť����������ɲ�
      *  03.01.01    NACL-����    06.07.14  ���Ŷ�ʬ�֤������ե饰���ɲ�
      *  03.05.01    NACL-����    07.06.01  ���롼�׿����б�
      *  04.04.01    NACL-����    10/03/29  �����������ɲ�
      *  04.05.01    NACL-����    10/01/19  �޶�ʬ�ɲ�
      *
      *  04.07.01    NACL-����    13/12/03  �����ա�ϫ�ҽ��ˤΥ쥻�ץ��ͼ�
      *                                     �ɲ��б�
      *
      *  04.08.01    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *
      *  05.01.01    ORCAMO       22/06/08  ��쥻�ץȤε��ܽ��б�
      *                                     �ʿ��Ŷ�ʬ������������
      *  05.02.01    ORCAMO       24/04/26  ���£�ǯ�ٲ����б�
      *                                     CDKBN-KOUBAN�򣲷夫�飳����ѹ�
      ************************************************************
      *01  RECE87-REC.
           03  RECE87-KEY.
               05  RECE87-KEY1.
      *���ŵ��أɣ�
                   07  RECE87-HOSPNUM     PIC  9(02).
      *����ǯ��
                   07  RECE87-SRYYM       PIC  9(06).
      *������ʬ
                   07  RECE87-NYUGAIKBN   PIC  X(01).
      *���ԣɣ�
                   07  RECE87-PTID        PIC  9(10).
      *�쥻�ץȲ�
                   07  RECE87-RECEKA      PIC  X(02).
      *�ݸ��ɣ�        
                   07  RECE87-HKNID       PIC  9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE87-TEKSTYMD    PIC  X(08). 
      *���Ŷ�ʬ
               05  RECE87-SRYKBN          PIC  X(02).
      *���ݶ�ʬ
               05  RECE87-JIHOKBN         PIC  X(01).
      *�޶�ʬ
               05  RECE87-ZAIKBN          PIC  9(03).
      *�޼��̶�ʬ
               05  RECE87-ZAISKBKBN       PIC  X(01).
      *�޼������ֹ�
               05  RECE87-ZAISKBBAN       PIC  9(08).
      *��ũ���϶�ʬ
               05  RECE87-TENTEKIKBN      PIC  9(02).
      *��ũ���϶�ʬ�����ֹ�
               05  RECE87-TENTEKIBAN      PIC  9(08).
      *�����»ܶ�ʬ
               05  RECE87-KNSJISKBN       PIC  9(01).
      *���ż��̶�ʬ
               05  RECE87-SRYSYUKBN       PIC  X(03).
      *������
               05  RECE87-DAY             PIC  9(02).
      *���Ŷ�ʬ�֤������ե饰�ʼ����ե���ѡ�
               05  RECE87-OKIKAEFLG       PIC  X(01).
      *ͥ���ʬ
               05  RECE87-PRIORITYKBN     PIC  9(01).
      *ɽ���ֹ�
               05  RECE87-CDKBN-SYO           PIC  9(01).
               05  RECE87-CDKBN-BU            PIC  9(02).
               05  RECE87-CDKBN-KBN           PIC  X(01).
               05  RECE87-CDKBN-KBNNUM        PIC  9(03).
               05  RECE87-CDKBN-KBNNUM-EDA    PIC  9(02).
               05  RECE87-CDKBN-KOUBAN        PIC  9(03).
               05  RECE87-KOUHYOJUNNUM        PIC  9(09).
      *��������
               05  RECE87-DAY2            PIC  9(02).
      *
               05  RECE87-NYUTENTTLKBN    PIC  9(03).
      *���ť����ɡʺޤ���Ƭ�ο��ť����ɡ�
               05  RECE87-SRYCD1          PIC  X(09).
      *������
               05  RECE87-ZAITEN          PIC  9(08).
      *���ֹ�
               05  RECE87-ZAINUM          PIC  9(08).
      *Ϣ�ֹ�
               05  RECE87-LINENUM         PIC  9(05).
      *�����ٿ�
           03  RECE87-ZAIMEISAISU         PIC  9(03).
      *�����ֹ�
           03  RECE87-PTNUM               PIC  X(20).
      *���Ų�
           03  RECE87-SRYKA               PIC  X(02).
      *Ŭ�������
           03  RECE87-TEKIYO-INF.
      *�޶��ڡʡ���
               05  RECE87-ASTER           PIC  X(02).
      *Ŧ��
               05  RECE87-TEKIYO          PIC  X(100).
      *Ŭ�������
           03  RECE87-TEKIYO2-INF.
               05  RECE87-TEKIYO2-KISAIFLG  PIC  9(01).
               05  RECE87-TEKIYO2-YENFLG    PIC  9(01).
               05  RECE87-TEKIYO2-TEN       PIC S9(07).
               05  RECE87-TEKIYO2-KAISU     PIC  9(03).
