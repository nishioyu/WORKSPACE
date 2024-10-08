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
      *    ������׺����ѥѥ�᥿������COPY (CPORCSNYUACCT.INC)
      *                                ���ѥץ������ (ORCGI01)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSNYUACCT-AREA.
      *
      *������׺�����ʬ�ʣ���������Ͽ�������ౡ��Ͽ�������ѹ�
      *                  ������ư��û���������׺��
      *                  ����������á�������פΤߡ�
      *                  ����������á�������פȿ��Ų�ס�
      *                �������ౡ��á�������ư�������������
      *                �������ౡ��Ͽ�ʿ�����¸�ˡ�
           03  LNK-SCNYUACCT-SYORI-KBN               PIC  X(01).
      *�����ֹ�
           03  LNK-SCNYUACCT-BTU-NUM                 PIC  X(02).
      *������������ʬ�ʣ�������������������������������
           03  LNK-SCNYUACCT-TOKUTEINYUIN-KBN        PIC  X(01).
      *������׺������֡ʳ��ϡ�
           03  LNK-SCNYUACCT-STSRYYMD                PIC  X(08).
      *������׺������֡ʽ�λ��
           03  LNK-SCNYUACCT-EDSRYYMD                PIC  X(08).
      *�������������ꥢ
           03  LNK-SCNYUACCT-NYUKHN.
               05  LNK-SCNYUACCT-NYUKHN-SRYCD        PIC  X(09)
                                                     OCCURS  6.
           03  LNK-SCNYUACCT-KAGENCD.
               05  LNK-SCNYUACCT-KAGENCD-TBL         OCCURS  50.
                   07  LNK-SCNYUACCT-KAGEN-SRYCD     PIC  X(09).
                   07  LNK-SCNYUACCT-KAGEN-STYUKYMD  PIC  X(08).
                   07  LNK-SCNYUACCT-KAGEN-EDYUKYMD  PIC  X(08).
      *�̻�������������������û������ѡ�
           03  LNK-SCNYUACCT-NYUKHN-NISUU            PIC  9(05).
      *�������û��ΰ�
           03  LNK-SCNYUACCT-NYUKHNKSN-AREA          OCCURS  30.
      *�������û���ʬ
             05  LNK-SCNYUACCT-NYUKHNKSN-KBN         PIC  X(01).
      *�������û����ť�����
             05  LNK-SCNYUACCT-NYUKHNKSN-SRYCD       PIC  X(09).
      *������ť�����
           03  LNK-SCNYUACCT-GAIHAKU-SRYCD           PIC  X(09).
      *�������ۿ��ť�����
           03  LNK-SCNYUACCT-SAGAKU-SRYCD            PIC  X(09).
      *�������۶�ʬ
           03  LNK-SCNYUACCT-SAGAKU-KBN              PIC  9(02).
      *�������ť�����
           03  LNK-SCNYUACCT-SHOKUJI-SRYCD           PIC  X(09).
      *������ʬ
           03  LNK-SCNYUACCT-SHOKUJIKBN              PIC  9(01).
      *�������åȶ�ʬ�ʶ���SHOKUJIKBN����ѡ�����ī����ο�����ʬ����ѡ�
           03  LNK-SCNYUACCT-SHOKUJI-SETKBN          PIC  X(01).
      *������ʬ��ī��
           03  LNK-SCNYUACCT-SHOKUJIKBN-ASA          PIC  9(01).
      *������ʬ�����
           03  LNK-SCNYUACCT-SHOKUJIKBN-HIRU         PIC  9(01).
      *������ʬ�����
           03  LNK-SCNYUACCT-SHOKUJIKBN-YORU         PIC  9(01).
      *��Ʋ�û�
           03  LNK-SCNYUACCT-SHOKUDO-SRYCD           PIC  X(09).
      *��Ʋ�û���ʬ
           03  LNK-SCNYUACCT-SHOKUDO-KBN             PIC  9(01).
      *�ݸ��ȹ祳����
           03  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD       PIC  X(09).
      *�ݸ��ȹ礻
           03  LNK-SCNYUACCT-HKNCOMBINUM             PIC  9(04).
      *ž�ʻ���������
           03  LNK-SCNYUACCT-MOTO-NYUINKA            PIC  X(02).
      *������������Ź԰٥�����
           03  LNK-SCNYUACCT-SENTEI-SRYCD            PIC  X(09).
      *���������񳫻���
           03  LNK-SCNYUACCT-SENTEI-STYMD            PIC  X(08).
      *�����������������ʬ
           03  LNK-SCNYUACCT-NYUCAL-KBN              PIC  9(01).
      *Ʊ����������������û��ο��Ų�׺����ʬ
      *�ʶ���Ʊ���������ʳ�������Ʊ����������
           03  LNK-SCNYUACCT-DOUJITSUKBN             PIC  X(01).
      *���Ų�׺��������
           03  LNK-SCNYUACCT-SRYDELYMD               PIC  X(08).
      *��ư��û���������׺�����ʬ
           03  LNK-SCNYUACCT-IDOSAKUSEI-KBN          PIC  X(01).
      *����������
           03  LNK-SCNYUACCT-RC                      PIC S9(9) BINARY.
      *
