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
      *         �����ޥ��� �������þ���   COPY (CPTENSUPLUS2.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  TENSUPLUS2-REC.
           03  TNSP2-KEY.
               05  TNSP2-HOSPNUM            PIC 9(2).
               05  TNSP2-SRYCD             PIC  X(09).
      *    Ĺ����Ϳ����
           03  TNSP2-CHOKI-NISSU           PIC  9(03).
      *    Ĺ����Ϳ����
           03  TNSP2-CHOKI-KIGEN           PIC  X(08).
      *    Ĺ����Ϳ���顼����
           03  TNSP2-CHOKIERR              PIC  9(01).
      *    ��Ϳ��
           03  TNSP2-TOUYORYO-G.
               05  TNSP2-TOUYORYO-T        OCCURS   10.
      *            ����ǯ��
                   07  TNSP2-TOUYOKGNAGE           PIC  X(02).
      *            ���ǯ��
                   07  TNSP2-TOUYOJGNAGE           PIC  X(02).
      *            ��Ϳ��
                   07  TNSP2-TOUYORYO1             PIC  9(05)V9(05).
      *            ��Ϳ�̡�������
                   07  TNSP2-TOUYORYO2             PIC  9(05)V9(05).
      *    ��Ϳ�̥��顼����
           03  TNSP2-TOUYOERR              PIC  9(01).
      *    ���������ʬ
           03  TNSP2-NAIFUKUKBN            PIC  9(01).
      *    ���²��
           03  TNSP2-TUKIJGNKAISU          PIC  9(3).
      *    ������Ϳ��
           03  TNSP2-TUKIJGNTOUYORYO       PIC  9(5)V9(5).
      *    Ϣ����������ॳ����
           03  TNSP2-YAKKAKJNCDR           PIC  X(12).
           03  TNSP2-TERMID                PIC  X(16).
           03  TNSP2-OPID                  PIC  X(16).
           03  TNSP2-CREYMD                PIC  X(08).
           03  TNSP2-UPYMD                 PIC  X(08).
           03  TNSP2-UPHMS                 PIC  X(06).
