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
      ******************************************************************
      *    ���Уɥꥯ������ͽ����󡡡�       COPY (CPAPPTXMLV2REQ.INC)
      ******************************************************************
        01  XML-APPOINTREQ.
            02  APNTREQ-CONTEXT            PIC S9(9)   BINARY.
            02  APNTREQ-OBJECT             PIC X(36).
            02  APNTREQ-MODE               PIC S9(9)   BINARY.
            02  APNTREQ-RECORDNAME         PIC X(128).
            02  APNTREQ-APPOINT.
      *       �����ֹ�
              03  APNTREQ-PATIENTID    PIC X(20).
      *       ͽ���̾
              03  APNTREQ-APPOINT-NAME     PIC X(100).
      *       ͽ�󥫥ʻ�̾
              03  APNTREQ-APPOINT-KANANAME PIC X(100).
      *       ͽ����
              03  APNTREQ-APPOINT-DATE PIC X(10).
      *       ͽ�����
              03  APNTREQ-APPOINT-TIME PIC X(8).
      *       ͽ��ɣ�
              03  APNTREQ-APPOINT-ID   PIC X(5).
      *       ���Ų�
              03  APNTREQ-APPOINT-DEP-CODE PIC X(2).
      *       �ɥ�����������
              03  APNTREQ-APPOINT-DR-CODE  PIC X(5).
      *       �������ƶ�ʬ������
              03  APNTREQ-APPOINT-DEP-INFO PIC X(2).
      *       ͽ�����ƥ�����
              03  APNTREQ-APPOINT-INFO PIC X(2).
      *       ͽ����
              03  APNTREQ-APPOINT-MEMO PIC X(100).
      *2023/12
      *       ��ʣ��ʬ
              03  APNTREQ-DUPLICATE-MODE   PIC X(03).
