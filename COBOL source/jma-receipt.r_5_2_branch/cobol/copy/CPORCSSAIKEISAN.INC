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
      *    ��ô��Ʒ׻��ѥѥ�᥿���������� COPY (CPORCSSAIKEISAN.INC)
      *                                ���ѥץ������  (ORCSSAIKEISAN)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  LNK-ORCSSAIKEISAN-REC.
      *
      *���ŵ��أɣ�
           03  LNK-ORCSSKS-HOSPNUM         PIC 9(2).
      *������ʬ
           03  LNK-ORCSSKS-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
           03  LNK-ORCSSKS-PTID            PIC  9(10).
      *�о���ɼ�ֹ�
           03  LNK-ORCSSKS-DENPNUM         PIC  9(07).
      *���Ų�
           03  LNK-ORCSSKS-SRYKA           PIC  X(02).
      *���Ʒ׻���
           03  LNK-ORCSSKS-IKTSORTKBN      PIC  X(01).
      *���Ʒ׻���ˡ
           03  LNK-ORCSSKS-IKTKEISANKBN    PIC  X(01).
      *ʬ�ڼ����ʬ
           03  LNK-ORCSSKS-BUNBENJIHIKBN   PIC  X(01).
      *
      *���Ʒ׻�����ʪ���շ׻��ե饰
           03  LNK-ORCSSKS-IKTGENKEIFLG    PIC  X(01).
      *���Ʒ׻����»ܥե饰
           03  LNK-ORCSSKS-IKTJISSIFLG     PIC  X(01).
      *���Ʒ׻��������ԥ����å��ե饰
           03  LNK-ORCSSKS-IKTCHECKFLG     PIC  X(01).
      *���Ʒ׻����ޥ�Ĺ������ôͥ��׻��ե饰
           03  LNK-ORCSSKS-IKTCYOKIFLG     PIC  X(01).
      *���Ʒ׻����ޤȤ�׻�����ե饰����ɼ�ֹ椬Ϣ³�Ǥʤ���
           03  LNK-ORCSSKS-MATOMEKJFLG     PIC  X(01).
      *
      *������
           03  LNK-ORCSSKS-SYSYMD          PIC  X(08).
      *��������
           03  LNK-ORCSSKS-SYSHMS          PIC  X(06).
      *
      *�ֵ���
      *
           03  LNK-ORCSSKS-OUT-AREA.
             04  LNK-ORCSSKS-OUT-OCC          OCCURS  15.
      *�׻���ɼ�ֹ�
               05  LNK-ORCSSKS-DENPNUM-END    PIC  9(07).
      *��Ǽ�ǡ���
               05  LNK-ORCSSKS-SYUNOU-DATA    PIC  X(2500).
      *����ԥե饰
               05  LNK-ORCSSKS-KOREIFLG       PIC  X(01).
      *
