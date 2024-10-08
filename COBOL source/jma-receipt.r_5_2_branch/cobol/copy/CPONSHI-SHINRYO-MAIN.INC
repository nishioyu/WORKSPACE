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
      *  ��ʳ�ǧ���ž����Ǽ��   (CPONSHI-SHINRYO-MAIN.INC)
      *****************************************************************
      *
      *01  ONSHI-SHINRYO-MAIN-REC.
           03  ONS-SNR-M-KEY.
      *���ŵ��ؼ����ֹ�
               05  ONS-SNR-M-HOSPNUM       PIC 9(02).
      *TBL_UUID
               05  ONS-SNR-M-TBL-UUID      PIC X(36).
      *���ԣɣ�
               05  ONS-SNR-M-PTID          PIC 9(10).
      *����ǯ��
               05  ONS-SNR-M-SRYYM         PIC X(06).
      *������ʬ�ʣ������ʡ�
               05  ONS-SNR-M-NYUGAIKBN     PIC X(01).
      *�ݸ�����
           03  ONS-SNR-M-HKNINF-G.
      *�ݸ����ֹ�
               05  ONS-SNR-M-HKNJANUM      PIC  X(08).
      *����
               05  ONS-SNR-M-KIGO          PIC  X(80).
      *�ֹ�
               05  ONS-SNR-M-NUM           PIC  X(80).
      *�ֹ����
               05  ONS-SNR-M-EDABAN        PIC  X(02).
      *���ŵ��إ�����
               05  ONS-SNR-M-HOSPCD        PIC  X(10).
      *���ŵ���̾
               05  ONS-SNR-M-HOSPNAME      PIC  X(80).
      *Ĵ�����ɥ�����
               05  ONS-SNR-M-CHOZAICD      PIC  X(10).
      *Ĵ������̾
               05  ONS-SNR-M-CHOZAINAME    PIC  X(80).
      ***
      *Ĵ�޵�����Ϣ��
           03  ONS-SNR-M-CHOZAI-SEQNUM PIC 9(03).
      *Ĵ�޵��ض�ʬ
           03  ONS-SNR-M-CHOZAI-KBN    PIC X(01).
      *����������Ϣ��
           03  ONS-SNR-M-SHOHO-SEQNUM  PIC 9(03).
      *�����ȯ�Ե��ض�ʬ
           03  ONS-SNR-M-SHOHO-KBN     PIC X(01).
      *������
           03  ONS-SNR-M-SRYDD-AREA.
               05  ONS-SNR-M-SRYDD     PIC X(02)
                                               OCCURS  31.
      *ü���ɣ�
           03  ONS-SNR-M-TERMID        PIC X(64).
      *���ڥ졼���ɣ�
           03  ONS-SNR-M-OPID          PIC X(16).
      *����ǯ����
           03  ONS-SNR-M-CREYMD        PIC X(08).
      *��������
           03  ONS-SNR-M-CREHMS        PIC X(06).
      *����ǯ����
           03  ONS-SNR-M-UPYMD         PIC X(08).
      *��������
           03  ONS-SNR-M-UPHMS         PIC X(06).
