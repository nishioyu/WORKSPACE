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
      *    ϫ�ҥ쥻�ץ����ٽ��������     COPY (CPRCF086.INC)    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-����    05.05.25  ���ż��̶�ʬ�����������ɲ�
      *  01.00.02    NACL-����    05.11.01  �ʥ��˥ե饰���ɲ�
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
      *01  RECE86-REC.
           03  RECE86-KEY.
      *���ŵ��أɣ�
               05  RECE86-HOSPNUM         PIC  9(02).
      *����ǯ��
               05  RECE86-SRYYM           PIC  9(06).
      *������ʬ
               05  RECE86-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  RECE86-PTID            PIC  9(10).
      *�쥻�ץȲ�
               05  RECE86-RECEKA          PIC  X(02).
      *�ݸ��ɣ�        
               05  RECE86-HKNID           PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE86-TEKSTYMD        PIC  X(08).
      *���ֹ�
               05  RECE86-ZAINUM          PIC  9(08).
      *Ϣ�ֹ�
               05  RECE86-LINENUM         PIC  9(05).
      *�����ٿ�
           03  RECE86-ZAIMEISAISU         PIC  9(03).
      *�����ֹ�
           03  RECE86-PTNUM               PIC  X(20).
      *���Ų�
           03  RECE86-SRYKA               PIC  X(02).
      *���Ŷ�ʬ
           03  RECE86-SRYKBN              PIC  X(02).
      *�޼��̶�ʬ
           03  RECE86-ZAISKBKBN           PIC  X(01).
      *�޼������ֹ�
           03  RECE86-ZAISKBBAN           PIC  9(08).
      *��ũ���϶�ʬ
           03  RECE86-TENTEKIKBN          PIC  9(02).
      *��ũ���϶�ʬ�����ֹ�
           03  RECE86-TENTEKIBAN          PIC  9(08).
      *Ŭ�������
           03  RECE86-TEKIYO-INF.
      *�޶��ڡʡ���
               05  RECE86-ASTER           PIC  X(02).
      *Ŧ��
               05  RECE86-TEKIYO          PIC  X(100).
      *������
           03  RECE86-ZAITEN              PIC  9(08).
      *�����»ܶ�ʬ
           03  RECE86-KNSJISKBN           PIC  9(01).
      *ɽ���ֹ�
           03  RECE86-CDKBN-SYO           PIC  9(01).
           03  RECE86-CDKBN-BU            PIC  9(02).
           03  RECE86-CDKBN-KBN           PIC  X(01).
           03  RECE86-CDKBN-KBNNUM        PIC  9(03).
           03  RECE86-CDKBN-KBNNUM-EDA    PIC  9(02).
           03  RECE86-CDKBN-KOUBAN        PIC  9(03).
           03  RECE86-KOUHYOJUNNUM        PIC  9(09).
      *���ż��̶�ʬ
           03  RECE86-SRYSYUKBN           PIC  X(03).
      *������
           03  RECE86-DAY                 PIC  9(02).
      *�ʥ��˥ե饰
           03  RECE86-SAFLG               PIC  X(01).
      *���Ŷ�ʬ�֤������ե饰�ʼ����ե���ѡ�
           03  RECE86-OKIKAEFLG           PIC  X(01).
      *�޶�ʬ
           03  RECE86-ZAIKBN              PIC  9(03).
      *��������
           03  RECE86-DAY2                PIC  9(02).
      *���ݶ�ʬ
           03  RECE86-JIHOKBN             PIC  X(01).
      *Ŭ�������
           03  RECE86-TEKIYO2-INF.
               05  RECE86-TEKIYO2-KISAIFLG  PIC  9(01).
               05  RECE86-TEKIYO2-YENFLG    PIC  9(01).
               05  RECE86-TEKIYO2-TEN       PIC S9(07).
               05  RECE86-TEKIYO2-KAISU     PIC  9(03).
      *ͥ���ʬ
           03  RECE86-PRIORITYKBN         PIC  9(01).
