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
      *****************************************************************
      *****************************************************************
      *  ��ʳ�ǧ���޾����Ǽ��   (CPONSHI-YAKUZAI-MAIN.INC)
      *****************************************************************
      *
      *01  ONSHI-YAKUZAI-MAIN-REC.
           03  ONS-YKZ-M-KEY.
      *���ŵ��ؼ����ֹ�
               05  ONS-YKZ-M-HOSPNUM       PIC 9(02).
      *TBL_UUID(2022/08)
               05  ONS-YKZ-M-TBL-UUID      PIC X(36).
      *���ԣɣ�
               05  ONS-YKZ-M-PTID          PIC 9(10).
      *����ǯ��
               05  ONS-YKZ-M-SRYYM         PIC X(06).
      *������ʬ�ʣ���Ĵ�ޡ�
               05  ONS-YKZ-M-NYUGAIKBN     PIC X(01).
      *�ݸ�����
           03  ONS-YKZ-M-HKNINF-G.
      *�ݸ����ֹ�
               05  ONS-YKZ-M-HKNJANUM      PIC  X(08).
      *����
               05  ONS-YKZ-M-KIGO          PIC  X(80).
      *�ֹ�
               05  ONS-YKZ-M-NUM           PIC  X(80).
      *�ֹ����
               05  ONS-YKZ-M-EDABAN        PIC  X(02).
      *2022/08
      *���ŵ��إ�����
               05  ONS-YKZ-M-HOSPCD        PIC  X(10).
      *���ŵ���̾
               05  ONS-YKZ-M-HOSPNAME      PIC  X(80).
      *Ĵ�����ɥ�����
               05  ONS-YKZ-M-CHOZAICD      PIC  X(10).
      *Ĵ������̾
               05  ONS-YKZ-M-CHOZAINAME    PIC  X(80).
      ***
      *Ĵ�޵�����Ϣ��
           03  ONS-YKZ-M-CHOZAI-SEQNUM PIC 9(03).
      *Ĵ�޵��ض�ʬ
           03  ONS-YKZ-M-CHOZAI-KBN    PIC X(01).
      *����������Ϣ��
           03  ONS-YKZ-M-SHOHO-SEQNUM  PIC 9(03).
      *�����ȯ�Ե��ض�ʬ
           03  ONS-YKZ-M-SHOHO-KBN     PIC X(01).
      *������
           03  ONS-YKZ-M-SRYDD-AREA.
               05  ONS-YKZ-M-SRYDD     PIC X(02)
                                               OCCURS  31.
      *ü���ɣ�
           03  ONS-YKZ-M-TERMID        PIC X(64).
      *���ڥ졼���ɣ�
           03  ONS-YKZ-M-OPID          PIC X(16).
      *����ǯ����
           03  ONS-YKZ-M-CREYMD        PIC X(08).
      *��������
           03  ONS-YKZ-M-CREHMS        PIC X(06).
      *����ǯ����
           03  ONS-YKZ-M-UPYMD         PIC X(08).
      *��������
           03  ONS-YKZ-M-UPHMS         PIC X(06).
