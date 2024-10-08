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
      *         �����ƥ�����ޥ���     (CPSK5002.INC)   *
      *         �ʣ���������                            *
      *         �¼��ޥ���                              *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-5002-REC.
           03  SYS-5002-KEY.
               05  SYS-5002-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-5002-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-5002-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-5002-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-5002-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-5002-TBL.
      *�¼��ֹ�
               05  SYS-5002-BRM-NUM
                                        PIC  X(08).
      *�¼�����
               05  SYS-5002-BRM-SBT
                                        PIC  X(02).
      *����������
               05  SYS-5002-BRM-TOKTEI-NYUIN
                                        PIC  X(02).
      *���ƿͿ�
               05  SYS-5002-BRM-NINZU   PIC  9(02).
      *��������
               05  SYS-5002-BRM-SAGAKU  PIC  X(02).
      *��������
               05  SYS-5002-BRM-SEX     PIC  X(01).
      *�����ֹ�
               05  SYS-5002-BRM-NAITEL
                                        PIC  X(05).
      *���Ų�
               05  SYS-5002-BRM-KA      PIC  X(02).
      *
      *�û��ΰ�
               05  SYS-5002-KSNAREA.
      *�������������Ŵ����û�
                   07  SYS-5002-SINSEIJI.
                       09  SYS-5002-SINSEIJI-CD    PIC X(09).
                       09  SYS-5002-SINSEIJI-KBN   PIC X(01).
      *���Ž������¾����ܴĶ��û�
                   07  SYS-5002-SNR-RYOYO          OCCURS 2.
                       09  SYS-5002-SNR-RYOYO-CD   PIC X(09).
                       09  SYS-5002-SNR-RYOYO-KBN  PIC X(01).
      *�����������¼������û�
                   07  SYS-5002-HOSYASEN.
                       09  SYS-5002-HOSYASEN-CD    PIC X(09).
                       09  SYS-5002-HOSYASEN-KBN   PIC X(01).
      *���ܴĶ��û�
                   07  SYS-5002-RYOYO.
                       09  SYS-5002-RYOYO-CD       PIC X(09).
                       09  SYS-5002-RYOYO-KBN      PIC X(01).
      *�����������ܴĶ��û�
      *������������󤫤��ư H15.02.27)
                   07  SYS-5002-BTU-RYOYO          OCCURS  3.
                       09  SYS-5002-BTU-RYOYO-CD   PIC X(09).
                       09  SYS-5002-BTU-RYOYO-KBN  PIC X(01).
      *�žɼ������ܴĶ����̲û��ʸļ���
                   07  SYS-5002-JYUTOK1.
                       09  SYS-5002-JYUTOK1-CD     PIC X(09).
                       09  SYS-5002-JYUTOK1-KBN    PIC X(01).
      *�žɼ������ܴĶ����̲û��ʣ���������
                   07  SYS-5002-JYUTOK2.
                       09  SYS-5002-JYUTOK2-CD     PIC X(09).
                       09  SYS-5002-JYUTOK2-KBN    PIC X(01).
      *̵�ݼ��ż������û�
                   07  SYS-5002-MUKIN.
                       09  SYS-5002-MUKIN-CD       PIC X(09).
                       09  SYS-5002-MUKIN-KBN      PIC X(01).
      *�����������ܴĶ��û���
                   07  SYS-5002-BTU-RYOYO-H18.
                       09  SYS-5002-BTU-RYOYO-H18-CD   PIC X(09).
                       09  SYS-5002-BTU-RYOYO-H18-KBN  PIC X(01).
      *�������û������
               05  SYS-5002-KSNTBLR  REDEFINES  SYS-5002-KSNAREA.
                 07  SYS-5002-KSN              OCCURS  12.
                   09  SYS-5002-KSNCD              PIC X(09).
                   09  SYS-5002-KSNKBN             PIC X(01).
      *�ϰ����           
               05  SYS-5002-BRM-CHIIKIHOU-KBN      PIC X(02).
               05  SYS-5002-BRM-CHIIKIHOU          PIC X(04).
               05  SYS-5002-YOBI                   PIC X(294).
               05  SYS-5002-KHNSRYCD               PIC X(09).
               05  SYS-5002-KHNKBNCD               PIC X(03).
               05  SYS-5002-TSRYCD                 PIC X(09).
               05  SYS-5002-YOBI2                  PIC X(35).
      *ü���ɣ�
           03  SYS-5002-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-5002-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-5002-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-5002-UPYMD           PIC  X(08).
      *��������
           03  SYS-5002-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-5002-UPD-KEY.
               05  SYS-5002-UPD-HOSPNUM     PIC 9(2).
               05  SYS-5002-UPD-KANRICD     PIC X(04).
               05  SYS-5002-UPD-KBNCD       PIC X(08).
               05  SYS-5002-UPD-STYUKYMD    PIC X(08).
               05  SYS-5002-UPD-EDYUKYMD    PIC X(08).
