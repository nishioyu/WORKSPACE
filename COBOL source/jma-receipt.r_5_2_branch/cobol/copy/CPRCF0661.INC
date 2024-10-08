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
      *    �쥻�ץ����ٽ�������� Ŭ�����¤��ؤ� (CPRCF066.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-����    05.05.25  ���ż��̶�ʬ�����������ɲ�
      *  03.05.01    NACL-��Ʀ��  07/05/30  ���롼�׿����б�
      *  04.04.01    NACL-����    10/03/29  �����������ɲ�
      *  04.05.01    NACL-����    10/01/12  �޶�ʬ�ɲ�
      *  04.05.02    NACL-����    11/06/20  �ݸ��ȹ礻�ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *
      *  04.07.01    NACL-ƣ��    12/12/17  ʣ����쥻�ץ�ʬ���б�
      *
      *  04.08.01    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *
      *  05.01.01    ORCAMO       22/06/08  ��쥻�ץȤε��ܽ��б�
      *                                     �ʿ��Ŷ�ʬ������������
      *  05.02.01    ORCAMO       24/04/26  ���£�ǯ�ٲ����б�
      *                                     CDKBN-KOUBAN�򣲷夫�飳����ѹ�
      ************************************************************
      *01  RECE661-REC.
           03  RECE661-KEY.
               05  RECE661-KEY1.
      *���ŵ��أɣ�
                   07  RECE661-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE661-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE661-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE661-PTID        PIC 9(10).
      *�쥻�ץȲ�
                   07  RECE661-RECEKA      PIC X(02).
      *������ʧ����
                   07  RECE661-TEISYUTUSAKI
                                           PIC 9(01).
      *�쥻�ץȼ���
                   07  RECE661-RECESYUBETU PIC 9(04).
      *�ݸ����ֹ�        
                   07  RECE661-HKNJANUM    PIC X(08).
      *�����ʬ
                   07  RECE661-HOJOKBN     PIC X(01).
      *����ɣ�
                   07  RECE661-KOHID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE661-TEKSTYMD    PIC X(08). 
      *ʬ���ʬ
                   07  RECE661-RECEIPT-KBN PIC X(10).
      *����ʬΥ��ʬ
               05  RECE661-KOHBUNRI        PIC  9(01).
      *���Ŷ�ʬ
               05  RECE661-SRYKBN          PIC  X(02).
      *���ݶ�ʬ
               05  RECE661-JIHOKBN         PIC  X(01).
      *�޶�ʬ
               05  RECE661-ZAIKBN          PIC  9(03).
      *�޼��̶�ʬ
               05  RECE661-ZAISKBKBN       PIC  X(01).
      *�޼������ֹ�
               05  RECE661-ZAISKBBAN       PIC  9(08).
      *��ũ���϶�ʬ
               05  RECE661-TENTEKIKBN      PIC  9(02).
      *��ũ���϶�ʬ�����ֹ�
               05  RECE661-TENTEKIBAN      PIC  9(08).
      *�����»ܶ�ʬ
               05  RECE661-KNSJISKBN       PIC  9(01).
      *���ż��̶�ʬ
               05  RECE661-SRYSYUKBN       PIC  X(03).
      *������
               05  RECE661-DAY             PIC  9(02).
      *ͥ���ʬ
               05  RECE661-PRIORITYKBN     PIC  9(01).
      *ɽ���ֹ�
               05  RECE661-CDKBN-SYO           PIC  9(01).
               05  RECE661-CDKBN-BU            PIC  9(02).
               05  RECE661-CDKBN-KBN           PIC  X(01).
               05  RECE661-CDKBN-KBNNUM        PIC  9(03).
               05  RECE661-CDKBN-KBNNUM-EDA    PIC  9(02).
               05  RECE661-CDKBN-KOUBAN        PIC  9(03).
               05  RECE661-KOUHYOJUNNUM        PIC  9(09).
      *��������
               05  RECE661-DAY2            PIC  9(02).
      *������
               05  RECE661-ZAITEN          PIC  9(08).
      *���ֹ�
               05  RECE661-ZAINUM          PIC  9(08).
      *Ϣ�ֹ�
               05  RECE661-LINENUM         PIC  9(05).
      *�����ٿ�
           03  RECE661-ZAIMEISAISU         PIC  9(03).
      *�����ֹ�
           03  RECE661-PTNUM               PIC  X(20).
      *���Ų�
           03  RECE661-SRYKA               PIC  X(02).
      *Ŭ�������
           03  RECE661-TEKIYO-INF.
      *�޶��ڡʡ���
               05  RECE661-ASTER           PIC  X(02).
      *Ŧ��
               05  RECE661-TEKIYO          PIC  X(100).
      *�ݸ��ȹ礻�ֹ�
           03  RECE661-HKNCOMBI            PIC  9(04).
