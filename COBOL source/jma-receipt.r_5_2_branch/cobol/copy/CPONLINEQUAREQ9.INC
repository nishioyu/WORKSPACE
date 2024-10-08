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
      *    ���Уɻ�ʳ�ǧ�������޽��������ֹ������    COPY (CPONLINEQUAREQ9.INC)*
      *****************************************************************
       01  XML-ONLINEQUAREQ9.
           03  QUAREQ9-CONTEXT             PIC S9(9)   BINARY.
           03  QUAREQ9-OBJECT              PIC X(36).
           03  QUAREQ9-MODE                PIC S9(9)   BINARY.
           03  QUAREQ9-RECORDNAME          PIC X(128).
           03  QUAREQ9-PATIENTREQ.
      *��å������إå�����     
               05  QUAREQ9-HEADER.
      *��ʳ�ǧ��
                   07  QUAREQ9-KAKU-KAKUNINYMD PIC X(08).
      *FUJYO_UUID
                   07  QUAREQ9-FUJYO-UUID      PIC X(36).
      *��å������ܥǥ���
      *��ʳ�ǧ���
               05  QUAREQ9-MESSAGEBODY.
      *�����޽���ʳ�ǧ��̥ꥹ��
                   07  QUAREQ9-MESSAGE-BODY-TBL1   OCCURS  2.
      *�ݸ����ֹ�
                       09  QUAREQ9-RES-HKNJANUM        PIC X(08).
      *���ݸ��Ծ��ֹ�
                       09  QUAREQ9-RES-NUM             PIC X(80).
      *�����޽���ʳ�ǧ��̥ꥹ��
      *
                       09  QUAREQ9-MESSAGE-BODY-TBL2   OCCURS   10.
      *���ŷ���Ĵ�޷���(01 :���ŷ� 02 :Ĵ�޷�)
                           11  QUAREQ9-TICKET-TYPE     PIC X(02).
      *���ŷ���Ĵ�޷�ͭ������ǯ����
                           11  QUAREQ9-MDC-TEKSTYMD   PIC X(08).
      *���ŷ���Ĵ�޷�ͭ����λǯ����
                           11  QUAREQ9-MDC-TEKEDYMD   PIC X(08).
      *�����ֹ�
                           11  QUAREQ9-KOFUNUM        PIC X(30).
      *����ǯ��
                           11  QUAREQ9-SRYYM           PIC X(06).
      *������ŵ��إ�����
                           11  QUAREQ9-DESIGN-CODE     PIC X(10).
      *������ŵ��س�ǧ�ե饰
                           11  QUAREQ9-DESIGN-FLG      PIC X(01).
      *������(1: ���� 2:������������ʳ��ϼΤ�(3:���� etc))
                           11  QUAREQ9-MEDICAL-TYPE     PIC X(02).
      *�ܿͻ�ʧ�ۡʼ�����ô�ۡ�
                           11  QUAREQ9-FTNGAKU         PIC X(06).
      *
                       09  QUAREQ9-IMAGE-FILE-STATUS   PIC X(01).
                       09  QUAREQ9-IMAGE-DATA          PIC X(20000).
