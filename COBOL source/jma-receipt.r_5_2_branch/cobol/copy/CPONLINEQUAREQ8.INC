      *****************************************************************
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      *****************************************************************
      *    ���Уɻ�ʳ�ǧ�����ʿ��ž�����    COPY (CPONLINEQUAREQ8.INC)*
      *****************************************************************
       01  XML-ONLINEQUAREQ8.
      ***  03  QUAREQ8-CONTEXT             PIC S9(9)   BINARY.
           03  QUAREQ8-OBJECT              PIC X(36).
           03  QUAREQ8-MODE                PIC S9(9)   BINARY.
      ***  03  QUAREQ8-RECORDNAME          PIC X(128).
           03  QUAREQ8-PATIENTREQ.
      *��å������إå�����     
               05  QUAREQ8-HEADER.
      *�׵��ݸ����ֹ�
                       09  QUAREQ8-REQ-HKNJANUM    PIC X(08).
      *����
                       09  QUAREQ8-REQ-KIGO        PIC X(80).
      *�ֹ�
                       09  QUAREQ8-REQ-NUM         PIC X(80).
      *�ֹ����
                       09  QUAREQ8-REQ-EDABAN      PIC X(02).
      *�׵�Ǥ�դΥե����뼱�̻�(���ŵ��ظ�ͭ����)
                       09  QUAREQ8-FILE-SIKIBETU   PIC X(50).
      *������̶�ʬ(����)
                       09  QUAREQ8-RESULTCODE      PIC X(01).
      *��å������ܥǥ���
               05  QUAREQ8-MESSAGEBODY.
                   07  QUAREQ8-MESSAGE-BODY-R.
      *����ǯ���̾���
      *����ǯ��
                       09  QUAREQ8-SRYYM         PIC X(06).
      *����������
                       09  QUAREQ8-NYUGAIKBN     PIC X(01).
      *���ŵ�����Ϣ��
                       09  QUAREQ8-GRPNUM        PIC X(03).
      *���ŵ��ض�ʬ
                       09  QUAREQ8-DIORG         PIC X(01).
      *����������Ϣ��
                       09  QUAREQ8-GRPNUM2       PIC X(03).
      *�����ȯ�Ե��ض�ʬ
                       09  QUAREQ8-PR-ISORG      PIC X(01).
      *���ŵ��ء����ɥ�����
                       09  QUAREQ8-PR-METRDIHCD  PIC X(10).
      *���ŵ��ء�����̾
                       09  QUAREQ8-PR-METRDIHNM  PIC X(80).
      *�����ȯ�԰��ŵ��إ�����
                       09  QUAREQ8-PR-PRLSHCD    PIC X(10).
      *�����ȯ�԰��ŵ���̾
                       09  QUAREQ8-PR-PRLSHNM    PIC X(80).
      ***
      *�������̾���
                       09  QUAREQ8-DI-DATE-REC.
      *�������̾���
                           11  QUAREQ8-DI-DATE-OCC.
      *������
                               13  QUAREQ8-DI-DATE      PIC X(08).
      *�����ȯ����
                               13  QUAREQ8-PR-DATE      PIC X(08).
      *���ž���
                               13  QUAREQ8-PR-DATA-INFG.
      *���ž���
                                   15  QUAREQ8-PR-DATA-INF
                                                         OCCURS  100.
      *���ż�������ʬ
                                       17  QUAREQ8-SRYKBN   PIC X(02).
      *���Ź԰٥�����
                                       17  QUAREQ8-SRYCD    PIC X(09).
      *���Ź԰�̾
                                       17  QUAREQ8-SRYNAME  PIC X(64).
      *ñ��
                                       17  QUAREQ8-UNIT     PIC X(12).
      *������
                                       17  QUAREQ8-QUA1     PIC X(11).
      *���̣�
                                       17  QUAREQ8-QUA2     PIC X(08).
      *���
                                       17  QUAREQ8-TIMES    PIC X(03).
      *���ʿ��Ź԰ٲû�����
                                       17  QUAREQ8-DEN-INF
                                                         OCCURS  30.
      *���Ź԰ٲû�������
                                           19  QUAREQ8-DEN-ADDCD
                                                          PIC X(05).
      *���Ź԰ٲû�̾��
                                           19  QUAREQ8-DEN-ADDNAME
                                                          PIC X(64).
      *���Ź԰ٲû�����
                                           19  QUAREQ8-DEN-QUA
                                                          PIC X(08).
      *ñ��
                                           19  QUAREQ8-DEN-UNIT
                                                          PIC X(12).



