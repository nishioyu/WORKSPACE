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
      *    �쥻�ץ����ٽ�     COPY (CPRCF06.INC)        *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01.06.29  �ݸ����ֹ桦
      *                                     Ŭ�ѳ���ǯ�������ɲ�
      *  01.00.02    NACL-ƣ��    03/03/12  �����ʬ���ɲ�
      *  01.00.03    NACL-����    03.03.03  ���ֹ�彤��
      *  01.00.04    NACL-����    05.02.28  �ʥ��˥ե饰���ɲ�
      *  01.00.05    NACL-����    05.05.25  ���ż��̶�ʬ�����������ɲ�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *  04.02.01    NACL-����    08/04/21  ��������Ƚ�̥ե饰�ɲ�
      *  04.04.01    NACL-����    10/03/29  �����������ɲ�
      *  04.05.01    NACL-����    10/01/12  �޶�ʬ�ɲ�
      *  04.05.02    NACL-����    11/06/20  �ݸ��ȹ礻�ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.02    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *
      *  04.08.01    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *
      *  05.01.01    ORCAMO       22/06/08  ��쥻�ץȤε��ܽ��б�
      *                                     �ʿ��Ŷ�ʬ������������
      *  05.02.01    ORCAMO       24/04/26  ���£�ǯ�ٲ����б�
      *                                     CDKBN-KOUBAN�򣲷夫�飳����ѹ�
      ************************************************************
      *01  RECE06-REC.
           03  RECE06-KEY.
               05  RECE06-KEY1.
      *���ŵ��أɣ�
                   07  RECE06-HOSPNUM      PIC 9(2).
      *����ǯ��
                   07  RECE06-SRYYM        PIC 9(06).
      *������ʬ
                   07  RECE06-NYUGAIKBN    PIC X(01).
      *���ԣɣ�
                   07  RECE06-PTID         PIC 9(10).
      *�쥻�ץȲ�
                   07  RECE06-RECEKA       PIC X(02).
      *������ʧ����
                   07  RECE06-TEISYUTUSAKI
                                           PIC 9(01).
      *�쥻�ץȼ���
                   07  RECE06-RECESYUBETU  PIC 9(04).
      *�ݸ����ֹ�        
                   07  RECE06-HKNJANUM     PIC X(08).
      *�����ʬ
                   07  RECE06-HOJOKBN      PIC X(01).
      *����ɣ�
                   07  RECE06-KOHID        PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE06-TEKSTYMD     PIC X(08). 
      *ʬ���ʬ
                   07  RECE06-RECEIPT-KBN  PIC X(10).
      *����ʬΥ��ʬ
               05  RECE06-KOHBUNRI        PIC  9(01).
      *���ֹ�
               05  RECE06-ZAINUM          PIC  9(08).
      *Ϣ�ֹ�
               05  RECE06-LINENUM         PIC  9(05).
      *�����ٿ�
           03  RECE06-ZAIMEISAISU         PIC  9(03).
      *�����ֹ�
           03  RECE06-PTNUM               PIC  X(20).
      *���Ų�
           03  RECE06-SRYKA               PIC  X(02).
      *���Ŷ�ʬ
           03  RECE06-SRYKBN              PIC  X(02).
      *Ŭ�������
           03  RECE06-TEKIYO-INF.
      *�޶��ڡʡ���
               05  RECE06-ASTER           PIC  X(02).
      *Ŧ��
               05  RECE06-TEKIYO          PIC  X(100).
      *������
           03  RECE06-ZAITEN              PIC  9(08).
      *�����»ܶ�ʬ
           03  RECE06-KNSJISKBN           PIC  9(01).
      *ɽ���ֹ�
           03  RECE06-CDKBN-SYO           PIC  9(01).
           03  RECE06-CDKBN-BU            PIC  9(02).
           03  RECE06-CDKBN-KBN           PIC  X(01).
           03  RECE06-CDKBN-KBNNUM        PIC  9(03).
           03  RECE06-CDKBN-KBNNUM-EDA    PIC  9(02).
           03  RECE06-CDKBN-KOUBAN        PIC  9(03).
           03  RECE06-KOUHYOJUNNUM        PIC  9(09).
      *�ʥ��˥ե饰
           03  RECE06-SAFLG               PIC  X(01).
      *���ż��̶�ʬ
           03  RECE06-SRYSYUKBN           PIC  X(03).
      *������
           03  RECE06-DAY                 PIC  9(02).
      *��������Ƚ�̥ե饰
           03  RECE06-SYOHOUFLG           PIC  9(01).
      *�޶�ʬ
           03  RECE06-ZAIKBN              PIC  9(03).
      *��������
           03  RECE06-DAY2                PIC  9(02).
      *�ݸ��ȹ礻�ֹ�
           03  RECE06-HKNCOMBI            PIC  9(04).
      *���ݶ�ʬ
           03  RECE06-JIHOKBN             PIC  X(01).
      *ͥ���ʬ
           03  RECE06-PRIORITYKBN         PIC  9(01).
