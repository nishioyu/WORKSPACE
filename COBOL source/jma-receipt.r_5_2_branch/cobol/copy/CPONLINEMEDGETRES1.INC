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
      *    ���Уɻ�ʳ�ǧ���ž��󡡼���   COPY (CPONLINEMEDGETRES1.INC)*
      *****************************************************************
       01  XML-ONLINEMEDGETRES1.
           03  MEDRES1-CONTEXT             PIC S9(9)   BINARY.
           03  MEDRES1-OBJECT              PIC X(36).
           03  MEDRES1-MODE                PIC S9(9)   BINARY.
           03  MEDRES1-RECORDNAME          PIC X(128).
           03  MEDRES1-PATIENTRES.
      *       �»���
              05  MEDRES1-INFORMATION-DATE         PIC X(10).
      *       �»ܻ���
              05  MEDRES1-INFORMATION-TIME         PIC X(08).
      *       �ֵѥ�����
              05  MEDRES1-API-RESULT               PIC X(03).
      *       �ֵѥ�å�����
              05  MEDRES1-API-RESULT-MSG           PIC X(100).
      *
              05  MEDRES1-RESKEY                   PIC X(100).
      *       ���Ծ���
              05  MEDRES1-PATIENT-INFOMATION.
      *           �����ֹ�
                  07  MEDRES1-PATIENTID            PIC X(20).
      *           ���Ի�̾
                  07  MEDRES1-NAME                 PIC X(100).
      *           ���ԥ��ʻ�̾
                  07  MEDRES1-KANANAME             PIC X(100).
      *           ������
                  07  MEDRES1-BIRTHDAY             PIC X(10).
      *           ����
                  07  MEDRES1-SEX                  PIC X(1).
      *
      *        ���ǯ��
               05  MEDRES1-SRYYM               PIC X(07).
      *�ꥯ�������ֹ�ᣰ��
      *        �����Ͽǯ��
               05  MEDRES1-DATA01-AREA.
                   07  MEDRES1-TOR-SRYYM-TBL       OCCURS   6.
                       09  MEDRES1-TOR-SRYYM       PIC X(07).
                       09  MEDRES1-TOR-HOSP-TBL    OCCURS   20. 
      *                    ���ŵ��ء����ɥ�����
                           11  MEDRES1-TOR-HOSPCD      PIC  X(10).
      *                    ���ŵ��ء�����̾
                           11  MEDRES1-TOR-HOSPNAME    PIC  X(80).
      *                    �����ȯ�԰��ŵ��إ�����
                           11  MEDRES1-TOR-CHOZAICD    PIC  X(10).
      *                    �����ȯ�԰��ŵ���̾
                           11  MEDRES1-TOR-CHOZAINAME  PIC  X(80).
      *                    ����������
                           11  MEDRES1-TOR-NYUGAIKBN   PIC X(01).


