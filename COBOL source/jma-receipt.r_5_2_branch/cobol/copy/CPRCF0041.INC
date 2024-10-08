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
      *    �쥻�ץ����ٽ�     COPY (CPRCF041.INC)                *
      *    (�쥻�ץ��Ż����������ƥ�Υ쥳�����¤��ؤ��ѥ��ԡ��� *      
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    02.09.13  �����Υơ��֥���ѹ�
      *  01.00.02    NACL-ƣ��    03/03/12  �����ʬ���ɲ�
      *  01.00.03    NACL-����    03.03.03  ���ֹ�彤��
      *
      *  02.09.01    NACL-ƣ��    05.04.17  ��������ǯ��������б�
      *
      *  03.03.01    NACL-ƣ��    06/09/29  ������Ƚ�����ε�����ˡ���ѹ�
      *                                     �������»�Ƚ�Ǥ��ɲ�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.03.01    NACL-����    08/07/14  ���쥻�����ʱ��������ޤ���б�
      *                                     �����������ե饰���ɲ�
      *
      *  04.05.01    NACL-ƣ��    09/06/10  Ŧ�������ؤΥ����ȵ����б�
      *                                     ���ż��̶�ʬ���ɲ�
      *  04.05.02    NACL-ƣ��    09/07/14  �������ʲ������б�
      *  04.05.03    NACL-ƣ��    09/11/27  �쥻�ť����ȳ�ĥ�б�
      *                                     �ط�������¸�ߥե饰�ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.02    NACL-ƣ��    11/01/05  ��ǽ�ɲá����û���Ʊ��ʻ������
      *  04.06.03    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *
      *  04.08.01    NACL-����    13/01/22  ��������ơ��֥룱�����б�
      *  04.08.02    NACL-ƣ��    16/04/20  ʿ������ǯ��������б�
      *  04.08.03    NACL-����    16/08/02  �����߰ʲ����ޤ�����ѥ쥻�ץȵ���
      *                                     �ʵ�Ͽ���б�
      ************************************************************
      *01  RECE041-REC.
           03  RECE041-KEY.
               05  RECE041-KEY1.
      *���ŵ��أɣ�
                   07  RECE041-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE041-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE041-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE041-PTID        PIC 9(10).
      *�쥻�ץȲ�
                   07  RECE041-RECEKA      PIC X(02).
      *������ʧ����
                   07  RECE041-TEISYUTUSAKI
                                           PIC 9(01).
      *�쥻�ץȼ���
                   07  RECE041-RECESYUBETU PIC 9(04).
      *�ݸ����ֹ�        
                   07  RECE041-HKNJANUM    PIC X(08).
      *�����ʬ
                   07  RECE041-HOJOKBN     PIC X(01).
      *����ɣ�
                   07  RECE041-KOHID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE041-TEKSTYMD    PIC X(08). 
      *ʬ���ʬ
                   07  RECE041-RECEIPT-KBN PIC X(10).
      *���Ŷ�ʬ
               05  RECE041-SRYKBN          PIC  X(02).
      *���ֹ�
               05  RECE041-ZAINUM          PIC  9(08).
      *����ʬΥ��ʬ
           03  RECE041-KOHBUNRI            PIC  9(01).
      *�����ֹ�
           03  RECE041-PTNUM               PIC  X(20).
      *���ż��̶�ʬ
           03  RECE041-SRYSYUKBN           PIC  X(03).
      *���Ų�
           03  RECE041-SRYKA               PIC  X(02).
      *�ݸ��ȹ礻�ֹ�
           03  RECE041-HKNCOMBI            PIC  9(04).
      *���ݶ�ʬ
           03  RECE041-JIHOKBN             PIC  X(01).
      *������
           03  RECE041-ZAITEN              PIC  9(08).
      *���ť����ɷ�
           03  RECE041-SRYCDTOTAL          PIC  9(14).
      *���̷�
           03  RECE041-SURYOUTOTAL         PIC  9(07)V9(05).
      *���ٿ�
           03  RECE041-MEISAISU            PIC  9(04).
      *�޲��
           03  RECE041-ZAIKAISU            PIC  9(04).
      *�굻������
           03  RECE041-SYUTEN1             PIC  9(07).
      *�굻������
           03  RECE041-SYUTEN2             PIC  9(07).
      *��������
           03  RECE041-YKZTEN              PIC  9(07).
      *�������
           03  RECE041-KIZAITEN            PIC  9(07).
      *�ɥ�����������
           03  RECE041-DRCD                PIC  X(05).
      *������ե饰
           03  RECE041-ZAIREQFLG           PIC  X(01).
      *����Ψ
           03  RECE041-TEIGENRATE          PIC  X(01).
      *�����������ʣ����ϣ��ʥ����ȤΤߡˡ�
           03  RECE041-TOYAKUFLG           PIC  X(01).
      *�ط�������¸�ߥե饰
           03  RECE041-SETCOMFLG           PIC  X(01).
      *�������»�Ƚ��
           03  RECE041-KNSJISKBN           PIC  9(01).
      *�������»�Ƚ�ǥ��롼��
           03  RECE041-KNSJISGRPKBN        PIC  9(02).
      *��������ơ��֥�
           03  RECE041-DAY-AREA.
      *        ���Ų���ơ��֥��1:��ס�2-9:���󤫤飹���
             04  RECE041-DAY-TBL           OCCURS  10.
                 05  RECE041-KAISUINF      OCCURS  31.
                     07  RECE041-KAISU     PIC  9(03).
      *
      *�굻������ơʼ�ѡ��������ǡ����֡�
           03  RECE041-ACCTP-SYUGI-TBL-G.
               05  RECE041-ACCTP-SYUGI-TBL  OCCURS  5.
      *�굻���ϥ�����
                   07  RECE041-ACCTP-SRYCD  PIC X(09).
      *�굻����
                   07  RECE041-ACCTP-SYUTEN PIC 9(07).
      *��������
                   07  RECE041-ACCTP-YKZTEN PIC 9(07).
      *����¾��������ʥե���ࡦ���ǡ����ǰʳ���
                   07  RECE041-ACCTP-KIZAITEN
                                           PIC 9(07).
      *�ե��������
                   07  RECE041-ACCTP-FIRTEN PIC 9(07).
      *��������
           03  RECE041-ACCTP-SANSOTEN       PIC 9(07).
      *��������
           03  RECE041-ACCTP-CHISOTEN       PIC 9(07).
      *
      *�������ե饰
           03  RECE041-SIPPUFLG             PIC X(01).
      *�����߰ʲ����ޥ쥻�ץȵ��ܥե饰
           03  RECE041-YKZKISAIFLG          PIC X(01).
