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
      *  ��ʳ�ǧ���ž����Ǽ��   (CPONSHI-SHINRYO-SUB.INC)
      *****************************************************************
      *
      *01  ONSHI-SHINRYO-SUB-REC.
           03  ONS-SNR-S-KEY.
      *���ŵ��ؼ����ֹ�
               05  ONS-SNR-S-HOSPNUM       PIC 9(02).
      *TBL_UUID
               05  ONS-SNR-S-TBL-UUID      PIC X(36).
      *���ԣɣ�
               05  ONS-SNR-S-PTID          PIC 9(10).
      *����ǯ��
               05  ONS-SNR-S-SRYYM         PIC X(06).
      *��������ʬ
               05  ONS-SNR-S-NYUGAIKBN     PIC X(01).
      *������
               05  ONS-SNR-S-SRYDD         PIC X(02).
      *�����ȯ����
               05  ONS-SNR-S-HAKKOYMD      PIC X(08).
      *�����ֹ�
               05  ONS-SNR-S-RENNUM        PIC 9(03).
      *
           03  ONS-SNR-S-DATA.
      *���ż�������ʬ
               05  ONS-SNR-S-SRYKBN        PIC X(02).
      *���ť�����
               05  ONS-SNR-S-SRYCD         PIC X(09).
      *����̾
               05  ONS-SNR-S-SRYNAME       PIC X(64).
      *ñ��
               05  ONS-SNR-S-TANINAME      PIC X(12).
      *������
               05  ONS-SNR-S-SURYO         PIC 9(05)V9(05).
      *���̣�
               05  ONS-SNR-S-SURYO2        PIC 9(05)V9(05).
      *���
               05  ONS-SNR-S-KAISU         PIC 9(03).
      *���ʲû�����̵ͭ
               05  ONS-SNR-SHIKA-KBN       PIC X(01).
      *ü���ɣ�
           03  ONS-SNR-S-TERMID        PIC X(64).
      *���ڥ졼���ɣ�
           03  ONS-SNR-S-OPID          PIC X(16).
      *����ǯ����
           03  ONS-SNR-S-CREYMD        PIC X(08).
      *��������
           03  ONS-SNR-S-CREHMS        PIC X(06).
      *����ǯ����
           03  ONS-SNR-S-UPYMD         PIC X(08).
      *��������
           03  ONS-SNR-S-UPHMS         PIC X(06).
      *
