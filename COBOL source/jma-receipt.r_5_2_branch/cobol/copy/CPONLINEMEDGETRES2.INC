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
      *    ���Уɻ�ʳ�ǧ���ž���ʰ�ʡ˼���   COPY (CPONLINEMEDGETRES2.INC)*
      *****************************************************************
       01  XML-ONLINEMEDGETRES2.
      *    03  MEDRES2-CONTEXT             PIC S9(9)   BINARY.
           03  MEDRES2-OBJECT              PIC X(36).
           03  MEDRES2-MODE                PIC S9(9)   BINARY.
      *    03  MEDRES2-RECORDNAME          PIC X(128).
           03  MEDRES2-PATIENTRES.
      *       �»���
              05  MEDRES2-INFORMATION-DATE         PIC X(10).
      *       �»ܻ���
              05  MEDRES2-INFORMATION-TIME         PIC X(08).
      *       �ֵѥ�����
              05  MEDRES2-API-RESULT               PIC X(03).
      *       �ֵѥ�å�����
              05  MEDRES2-API-RESULT-MSG           PIC X(100).
      *
     ***      05  MEDRES2-RESKEY                   PIC X(100).
      *       ���Ծ���
              05  MEDRES2-PATIENT-INFOMATION.
      *           �����ֹ�
                  07  MEDRES2-PATIENTID            PIC X(20).
      *           ���Ի�̾
                  07  MEDRES2-NAME                 PIC X(100).
      *           ���ԥ��ʻ�̾
                  07  MEDRES2-KANANAME             PIC X(100).
      *           ������
                  07  MEDRES2-BIRTHDAY             PIC X(10).
      *           ����
                  07  MEDRES2-SEX                  PIC X(1).
      *
      *�ݸ�����
                   07  MEDRES2-HKNINF-G.
      *�ݸ����ֹ�
                       09  MEDRES2-HKNJANUM          PIC  X(08).
      *����
                       09  MEDRES2-KIGO              PIC  X(80).
      *�ֹ�
                       09  MEDRES2-NUM               PIC  X(80).
      *�ֹ����
                       09  MEDRES2-EDABAN            PIC  X(02).
      *
      *        �����Ͽǯ��
               05  MEDRES2-DATA02-AREA.
                   07  MEDRES2-MEISA-AREA.
      *����������
                       09  MEDRES2-NYUGAIKBN           PIC  X(01).
      *���ŵ��ء����ɥ�����
                       09  MEDRES2-HOSPCD              PIC  X(10).
      *���ŵ��ء�����̾
                       09  MEDRES2-HOSPNAME            PIC  X(80).
      *�����ȯ�԰��ŵ��إ�����
                       09  MEDRES2-CHOZAICD            PIC  X(10).
      *�����ȯ�԰��ŵ���̾
                       09  MEDRES2-CHOZAINAME          PIC  X(80).
      *
                       09  MEDRES2-DATE-REC.
      *������
                           11  MEDRES2-DI-DATE       PIC X(10).
      *�����ȯ����
                           11  MEDRES2-PR-DATE       PIC X(10).
      *���ž���
                           11  MEDRES2-PR-DATA-INFG.
      *���ž���
                               13  MEDRES2-PR-DATA-INF
                                                         OCCURS  200.
      *���ż�������ʬ
                                   15  MEDRES2-SRYKBN   PIC X(02).
      *���Ź԰٥�����
                                   15  MEDRES2-SRYCD    PIC X(09).
      *���Ź԰�̾
                                   15  MEDRES2-SRYNAME  PIC X(64).
      *ñ��
                                   15  MEDRES2-TANINAME PIC X(12).
      *������
                                   15  MEDRES2-SURYO    PIC X(11).
      *���̣�
                                   15  MEDRES2-SURYO2   PIC X(11).
      *���
                                   15  MEDRES2-KAISU    PIC X(03).

