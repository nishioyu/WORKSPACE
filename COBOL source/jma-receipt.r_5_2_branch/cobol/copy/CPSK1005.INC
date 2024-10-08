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
      *         �����ƥ�����ޥ���     (CPSK1005.INC)   *
      *         �ʣ���������                            *
      *         ���Ų��ܾ���                            *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 04.07.00     NACL-¿��Ǽ  13/03/XX  ���ֳ��û����������ɲ�
      ************************************************************
       01  SYS-1005-REC.
           03  SYS-1005-KEY.
               05  SYS-1005-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1005-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1005-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1005-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1005-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1005-TBL.
      *���Ų�̾
               05  SYS-1005-SRYKANAME   PIC  X(80).
      *û��̾��
               05  SYS-1005-SRYKANAME1  PIC  X(20).
      *û��̾��
               05  SYS-1005-SRYKANAME2  PIC  X(12).
      *û��̾��
               05  SYS-1005-SRYKANAME3  PIC  X(04).
      *�쥻�Ż����Ųʡʥ����ɡ�
               05  SYS-1005-RECESRYKA   PIC  X(02).
      *
      *���ֻ��ֳ��û���������
               05  SYS-1005-JIKANKSN1-40     PIC  X(01).
      *��ѻ��ֳ��û���������
               05  SYS-1005-JIKANKSN1-50     PIC  X(01).
      *R04.4
      *��ɡ���������Ļ����ֲû�����
               05  SYS-1005-JIBINYUKSN       PIC  X(01).
      *ͽ��
      **   03  SYS-1005-YOBI            PIC  X(382).
      *ͽ��
      **   03  SYS-1005-YOBI            PIC  X(380).
           03  SYS-1005-YOBI            PIC  X(379).
      *ü���ɣ�
           03  SYS-1005-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1005-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1005-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1005-UPYMD           PIC  X(08).
      *��������
           03  SYS-1005-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1005-UPD-KEY.
               05  SYS-1005-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1005-UPD-KANRICD     PIC X(04).
               05  SYS-1005-UPD-KBNCD       PIC X(08).
               05  SYS-1005-UPD-STYUKYMD    PIC X(08).
               05  SYS-1005-UPD-EDYUKYMD    PIC X(08).
