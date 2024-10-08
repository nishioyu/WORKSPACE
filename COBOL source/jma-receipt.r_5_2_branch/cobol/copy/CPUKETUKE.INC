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
      *****************************************************************
      *  ����               (CPUKETUKE.INC) 
      *****************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  UKETUKE-REC.
      *
      *����
           03  UKE-KEY.
      *���ŵ��أɣ�
               05  UKE-HOSPNUM          PIC 9(2).
      *����ǯ����
               05  UKE-UKEYMD          PIC  X(08).
      *���գɣ�
               05  UKE-UKEID           PIC  9(05).
      *���ջ���
           03  UKE-UKETIME             PIC  X(06).
      *���ԣɣ�
           03  UKE-PTID                PIC  9(10).
      *���Ի�̾
           03  UKE-NAME                PIC X(100).
      *���Ų�
           03  UKE-SRYKA               PIC X(02).
      *�ɥ�����������
           03  UKE-DRCD                PIC X(05).
      *��������
           03  UKE-SRYNAIYO            PIC X(02).
      *�ݸ��ȹ礻
           03  UKE-HKNCOMBI            PIC 9(04).
      *��׻���
           03  UKE-KAIKEITIME          PIC 9(04).
      *����ͽ�����
           03  UKE-KEYYYKTIME          PIC 9(04).
      *ͽ�����
           03  UKE-YYKTIME             PIC 9(04).
      *ͽ��ɣ�
           03  UKE-YYKID               PIC 9(02).
      *ɽ����
           03  UKE-HYOJIJUN            PIC 9(04).
      *��ǧϢ��
           03  UKE-KAKREN              PIC X(02).
      *�������ե饰
           03  UKE-SRFLG               PIC X(02).
      *ü���ɣ�
           03  UKE-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  UKE-OPID                PIC  X(16).
      *����ǯ����
           03  UKE-CREYMD              PIC  X(08).
      *����ǯ����
           03  UKE-UPYMD               PIC  X(08).
      *��������
           03  UKE-UPHMS               PIC  X(06).
      *OFFSET
           03  UKE-UKEOFFSET           PIC 9(08).
