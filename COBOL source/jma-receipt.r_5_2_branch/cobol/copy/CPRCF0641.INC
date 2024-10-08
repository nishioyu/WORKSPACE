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
      *    �쥻�ץ����ٽ�     COPY (CPRCF064.INC)                *
      *    (�쥻�ץ��Ż����������ƥ�Υ쥳�����¤��ؤ��ѥ��ԡ��� *      
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  02.09.01    NACL-ƣ��    05.04.17  ��������ǯ��������б�
      *  03.03.01    NACL-ƣ��    06/09/29  ������Ƚ�����ε�����ˡ���ѹ�
      *                                     �������»�Ƚ�Ǥ��ɲ�
      *  03.05.01    NACL-��Ʀ��  07/05/30  ���롼�׿����б�
      * 
      *  04.02.01    NACL-����    08/04/10  �쥻�����������û��ե饰�ɲ�
      *
      *  04.03.01    NACL-����    08/07/14  ���쥻�����ʱ��������ޤ���б�
      *                                     �����������ե饰���ɲ�
      *
      *  04.05.01    NACL-ƣ��    09/06/10  Ŧ�������ؤΥ����ȵ����б�
      *                                     ���ż��̶�ʬ���ɲ�
      *  04.05.02    NACL-ƣ��    09/07/14  �������ʲ������б�
      *  04.05.03    NACL-ƣ��    09/11/27  �쥻�ť����ȳ�ĥ�б�
      *                                     �ط�������¸�ߥե饰�ɲ�
      *  04.05.03    NACL-ƣ��    10/01/13  �����б�
      *                                     �޶�ʬ���ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.02    NACL-ƣ��    11/01/05  ��ǽ�ɲá����û���Ʊ��ʻ������
      *  04.06.03    NACL-ƣ��    11/01/28  �����˺�Ϣ���ɲ�
      *
      *  04.07.01    NACL-ƣ��    12/12/17  ʣ����쥻�ץ�ʬ���б�
      *  04.07.02    NACL-����    15/08/10  �����ݸ��ȹ礻�ֹ��ɲ�
      *  04.08.01    NACL-����    16/08/02  �����߰ʲ����ޤ�����ѥ쥻�ץȵ���
      *                                     �ʵ�Ͽ���б�
      *  04.08.02    NACL-����    16/08/23  �������б�
      *  05.02.01    ORCAMO       23/05/29  �ţƿ��Ų��б�
      ************************************************************
      *01  RECE641-REC.
           03  RECE641-KEY.
               05  RECE641-KEY1.
      *���ŵ��أɣ�
                   07  RECE641-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE641-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE641-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE641-PTID        PIC 9(10).
      *�쥻�ץȲ�
                   07  RECE641-RECEKA      PIC X(02).
      *������ʧ����
                   07  RECE641-TEISYUTUSAKI
                                           PIC 9(01).
      *�쥻�ץȼ���
                   07  RECE641-RECESYUBETU PIC 9(04).
      *�ݸ����ֹ�        
                   07  RECE641-HKNJANUM    PIC X(08).
      *�����ʬ
                   07  RECE641-HOJOKBN     PIC X(01).
      *����ɣ�
                   07  RECE641-KOHID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE641-TEKSTYMD    PIC X(08). 
      *ʬ���ʬ
                   07  RECE641-RECEIPT-KBN PIC X(10).
      *���Ŷ�ʬ
               05  RECE641-SRYKBN          PIC  X(02).
      *������ɽ���ֹ�ݶ�ʬ�ֹ�
               05  RECE641-CDKBN-KBNNUM    PIC  9(03).
      *���ֹ�
               05  RECE641-ZAINUM          PIC  9(08).
      *��Ϣ��
               05  RECE641-ZAIREN          PIC  9(02).
      *����ʬΥ��ʬ
           03  RECE641-KOHBUNRI            PIC  9(01).
      *�����ֹ�
           03  RECE641-PTNUM               PIC  X(20).
      *���ż��̶�ʬ
           03  RECE641-SRYSYUKBN           PIC  X(03).
      *���Ų�
           03  RECE641-SRYKA               PIC  X(02).
      *�ݸ��ȹ礻�ֹ�
           03  RECE641-HKNCOMBI            PIC  9(04).
      *���ݶ�ʬ
           03  RECE641-JIHOKBN             PIC  X(01).
      *������
           03  RECE641-ZAITEN              PIC  9(08).
      *���ť����ɷ�
           03  RECE641-SRYCDTOTAL          PIC  9(14).
      *���̷�
           03  RECE641-SURYOUTOTAL         PIC  9(07)V9(05).
      *���ť����ɡ����̤��߷�
           03  RECE641-RUIKEITOTAL         PIC  9(13)V9(05).
      *���ٿ�
           03  RECE641-MEISAISU            PIC  9(04).
      *�޲��
           03  RECE641-ZAIKAISU            PIC  9(04).
      *�굻������
           03  RECE641-SYUTEN1             PIC  9(07).
      *�굻������
           03  RECE641-SYUTEN2             PIC  9(07).
      *��������
           03  RECE641-YKZTEN              PIC  9(07).
      *�������
           03  RECE641-KIZAITEN            PIC  9(07).
      *�ɥ�����������
           03  RECE641-DRCD                PIC  X(05).
      *������ե饰
           03  RECE641-ZAIREQFLG           PIC  X(01).
      *����Ψ
           03  RECE641-TEIGENRATE          PIC  X(01).
      *�����������ʣ����ϣ��ʥ����ȤΤߡˡ�
           03  RECE641-TOYAKUFLG           PIC  X(01).
      *�ط�������¸�ߥե饰
           03  RECE641-SETCOMFLG           PIC  X(01).
      *�޶�ʬ
           03  RECE641-ZAIKBN              PIC  9(03).
      *�������»�Ƚ��
           03  RECE641-KNSJISKBN           PIC  9(01).
      *�������»�Ƚ�ǥ��롼��
           03  RECE641-KNSJISGRPKBN        PIC  9(02).
      *�쥻�����������û��ե饰
           03  RECE641-NYUKSNFLG           PIC  9(01).
      *��������ơ��֥�
           03  RECE641-DAY-AREA.
      *        ���Ų���ơ��֥��1:���,2:����,3:����, 4:����ʾ��
             04  RECE641-DAY-TBL           OCCURS  4.
                 05  RECE641-KAISUINF      OCCURS  31.
                     07  RECE641-KAISU     PIC  9(03).
      *
      *�굻������ơʼ�ѡ��������ǡ����֡�
           03  RECE641-ACCTP-SYUGI-TBL-G.
               05  RECE641-ACCTP-SYUGI-TBL  OCCURS  5.
      *�굻���ϥ�����
                   07  RECE641-ACCTP-SRYCD  PIC X(09).
      *�굻����
                   07  RECE641-ACCTP-SYUTEN PIC 9(07).
      *��������
                   07  RECE641-ACCTP-YKZTEN PIC 9(07).
      *����¾��������ʥե���ࡦ���ǡ����ǰʳ���
                   07  RECE641-ACCTP-KIZAITEN
                                           PIC 9(07).
      *�ե��������
                   07  RECE641-ACCTP-FIRTEN PIC 9(07).
      *��������
           03  RECE641-ACCTP-SANSOTEN       PIC 9(07).
      *��������
           03  RECE641-ACCTP-CHISOTEN       PIC 9(07).
      *
      *�����ݸ��ȹ礻�ֹ�
           03  RECE641-HKNCOMBI-DAY-AREA.
               05  RECE641-HKNCOMBI-DAY     PIC 9(04)     OCCURS  31.
      *
      *�ƿǲû��������ˤκ�
           03  RECE641-NYUSAIFLG            PIC X(01).
      *�����߰ʲ����ޥ쥻�ץȵ��ܥե饰
           03  RECE641-YKZKISAIFLG          PIC X(01).
      *��������ʬ
           03  RECE641-JISANYAKKBN          PIC X(01).
      *
      *�ţƥե�������Ųʥơ��֥�ե饰
           03  RECE641-EFSRYKAFLG           PIC X(01).
      *�ţƥե�������Ųʥơ��֥�
           03  RECE641-EFSRYKA-AREA.
      *      ���Ų���ơ��֥��1:����,2:����,3:����ʾ��
             04  RECE641-EFSRYKA-TBL           OCCURS   3.
                 05  RECE641-EFSRYKA-INF       OCCURS  31.
                     07  RECE641-EFSRYKA       PIC  X(02).
      *
