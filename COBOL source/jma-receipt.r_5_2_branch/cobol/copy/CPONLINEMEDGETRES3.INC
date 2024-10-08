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
      *    ���Уɻ�ʳ�ǧ���ž���ʻ��ʡˡ�����   COPY (CPONLINEMEDGETRE3.INC)*
      *****************************************************************
       01  XML-ONLINEMEDGETRES3.
      ***  03  MEDRES3-CONTEXT             PIC S9(9)   BINARY.
           03  MEDRES3-OBJECT              PIC X(36).
           03  MEDRES3-MODE                PIC S9(9)   BINARY.
      **** 03  MEDRES3-RECORDNAME          PIC X(128).
           03  MEDRES3-PATIENTRES.
      *       �»���
              05  MEDRES3-INFORMATION-DATE         PIC X(10).
      *       �»ܻ���
              05  MEDRES3-INFORMATION-TIME         PIC X(08).
      *       �ֵѥ�����
              05  MEDRES3-API-RESULT               PIC X(03).
      *       �ֵѥ�å�����
              05  MEDRES3-API-RESULT-MSG           PIC X(100).
      *
      ***     05  MEDRES3-RESKEY                   PIC X(100).
      *       ���Ծ���
              05  MEDRES3-PATIENT-INFOMATION.
      *           �����ֹ�
                  07  MEDRES3-PATIENTID            PIC X(20).
      *           ���Ի�̾
                  07  MEDRES3-NAME                 PIC X(100).
      *           ���ԥ��ʻ�̾
                  07  MEDRES3-KANANAME             PIC X(100).
      *           ������
                  07  MEDRES3-BIRTHDAY             PIC X(10).
      *           ����
                  07  MEDRES3-SEX                  PIC X(1).
      *
      *�ݸ�����
                   07  MEDRES3-HKNINF-G.
      *�ݸ����ֹ�
                       09  MEDRES3-HKNJANUM          PIC  X(08).
      *����
                       09  MEDRES3-KIGO              PIC  X(80).
      *�ֹ�
                       09  MEDRES3-NUM               PIC  X(80).
      *�ֹ����
                       09  MEDRES3-EDABAN            PIC  X(02).
      *
      *        �����Ͽǯ��
               05  MEDRES3-DATA02-AREA.
                   07  MEDRES3-MEISA-AREA.
      *����������
                       09  MEDRES3-NYUGAIKBN           PIC  X(01).
      *���ŵ��ء����ɥ�����
                       09  MEDRES3-HOSPCD              PIC  X(10).
      *���ŵ��ء�����̾
                       09  MEDRES3-HOSPNAME            PIC  X(80).
      *�����ȯ�԰��ŵ��إ�����
                       09  MEDRES3-CHOZAICD            PIC  X(10).
      *�����ȯ�԰��ŵ���̾
                       09  MEDRES3-CHOZAINAME          PIC  X(80).
      *
                       09  MEDRES3-DATE-REC.
      *������
                           11  MEDRES3-DI-DATE       PIC X(10).
      *�����ȯ����
                           11  MEDRES3-PR-DATE       PIC X(10).
      *���ž���
                           11  MEDRES3-PR-DATA-INFG.
      *���ž���
                               13  MEDRES3-PR-DATA-INF
                                                         OCCURS  100.
      *���ż�������ʬ
                                   15  MEDRES3-SRYKBN   PIC X(02).
      *���Ź԰٥�����
                                   15  MEDRES3-SRYCD    PIC X(09).
      *���Ź԰�̾
                                   15  MEDRES3-SRYNAME  PIC X(64).
      *ñ��
                                   15  MEDRES3-TANINAME PIC X(12).
      *������
                                   15  MEDRES3-SURYO    PIC X(11).
      *���̣�
                                   15  MEDRES3-SURYO2   PIC X(11).
      *���
                                   15  MEDRES3-KAISU    PIC X(03).
      *���ʾ���
                                   15  MEDRES3-SHIKA-G  OCCURS 30.
                                       17  MEDRES3-SRYKASANCD
                                                            PIC X(05).
                                       17  MEDRES3-SRYKASAN-NAME
                                                            PIC X(64).
                                       17  MEDRES3-SHIKA-SURYO
                                                            PIC X(11).
                                       17  MEDRES3-SHIKA-TANINAME
                                                            PIC X(12).

