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
      *    �쥻�ץ����ٽ�     COPY (CPRCF046.INC)                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-����    03.03.03  ���ֹ�彤��
      *  01.00.02    NACL-����    05.07.28  �ޥ饹�ȥե饰���ɲ�
      *  03.05.01    NACL-����    07/05/28  ���롼�׿����б�
      *  04.01.01    NACL-����    07/11/26  �����ྦ��̾�����б�
      *
      *  04.05.01    NACL-ƣ��    09/07/14  �������ʲ������б�
      ************************************************************
      *01  RECE461-REC.
           03  RECE461-KEY.
      *���ŵ��أɣ�
               05  RECE461-HOSPNUM         PIC  9(02).
      *����ǯ��
               05  RECE461-SRYYM           PIC  9(06).
      *������ʬ
               05  RECE461-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  RECE461-PTID            PIC  9(10).
      *�쥻�ץȲ�
               05  RECE461-RECEKA          PIC  X(02).
      *�ݸ��ɣ�        
               05  RECE461-HKNID           PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE461-TEKSTYMD        PIC  X(08).
      *���Ŷ�ʬ
               05  RECE461-SRYKBN          PIC  X(02).
      *
               05  RECE461-GAITENTTLKBN    PIC  9(03).
      *���ť����ɡʺޤ���Ƭ�ο��ť����ɡ�
               05  RECE461-SRYCD1          PIC  X(09).                    
      *���ֹ�
               05  RECE461-ZAINUM          PIC  9(08).
      *Ϣ�ֹ�
               05  RECE461-LINENUM         PIC  9(05).
      *�����ֹ�
           03  RECE461-PTNUM               PIC  X(20).
      *���Ų�
           03  RECE461-SRYKA               PIC  X(02).
      *�޲��
           03  RECE461-ZAIKAISU            PIC  9(04).
      *�ޥ饹�ȥե饰
           03  RECE461-ZAILAST             PIC  9(01).
      *���Ź԰پ���
           03  RECE461-SRYACTINF.
      *���ť�����
               05  RECE461-SRYCD           PIC  X(09).
      *����
               05  RECE461-SRYSURYO        PIC  9(05)V9(05).
      *���
               05  RECE461-SRYKAISU        PIC  9(03).
      *ñ�̥�����
               05  RECE461-TANICD          PIC  9(03).
      *ñ��̾��ͭ�����
               05  RECE461-TANIMOJISU      PIC  9(01).
      *ñ��̾��
               05  RECE461-TANINAME        PIC  X(24).
      *�������̶�ʬ
               05  RECE461-TENSIKIBETU     PIC  9(01).
      *����
               05  RECE461-TEN             PIC  9(09)V9(02).
      *��������ե饰
               05  RECE461-MEISKYFLG       PIC  X(01).
      *��ư�����ʬ
               05  RECE461-AUTOKBN         PIC  X(01).
      *̾�������ֹ�
               05  RECE461-INPUTNUM        PIC  9(03).
      *�����󽸷��輱��
               05  RECE461-TENTTLKBN       PIC  9(03).
      *̾�ξ���
           03  RECE461-NAMEINF.
      *����ͭ�����
               05  RECE461-YUKOKETA        PIC  9(03).
      *����̾��
               05  RECE461-NAME            PIC  X(300).
