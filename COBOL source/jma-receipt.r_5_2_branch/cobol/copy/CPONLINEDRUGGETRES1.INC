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
      *    ���Уɻ�ʳ�ǧ���޾��󡡼���   COPY (CPONLINEDRUGGETRES1.INC)*
      *****************************************************************
       01  XML-ONLINEDRUGGETRES1.
           03  DRGRES1-CONTEXT             PIC S9(9)   BINARY.
           03  DRGRES1-OBJECT              PIC X(36).
           03  DRGRES1-MODE                PIC S9(9)   BINARY.
           03  DRGRES1-RECORDNAME          PIC X(128).
           03  DRGRES1-PATIENTRES.
      *       �»���
              05  DRGRES1-INFORMATION-DATE         PIC X(10).
      *       �»ܻ���
              05  DRGRES1-INFORMATION-TIME         PIC X(08).
      *       �ֵѥ�����
              05  DRGRES1-API-RESULT               PIC X(03).
      *       �ֵѥ�å�����
              05  DRGRES1-API-RESULT-MSG           PIC X(100).
      *
              05  DRGRES1-RESKEY                   PIC X(100).
      *       ���Ծ���
              05  DRGRES1-PATIENT-INFOMATION.
      *           �����ֹ�
                  07  DRGRES1-PATIENTID            PIC X(20).
      *           ���Ի�̾
                  07  DRGRES1-NAME                 PIC X(100).
      *           ���ԥ��ʻ�̾
                  07  DRGRES1-KANANAME             PIC X(100).
      *           ������
                  07  DRGRES1-BIRTHDAY             PIC X(10).
      *           ����
                  07  DRGRES1-SEX                  PIC X(1).
      *
      *        ���ǯ��
               05  DRGRES1-SRYYM               PIC X(07).
      *�ꥯ�������ֹ�ᣰ��
      *        �����Ͽǯ��
               05  DRGRES1-DATA01-AREA.
                   07  DRGRES1-TOR-SRYYM-TBL       OCCURS   12.
                       09  DRGRES1-TOR-SRYYM       PIC X(07).
                       09  DRGRES1-TOR-HOSP-TBL    OCCURS   20. 
      *                    ���ŵ��إ�����
                           11  DRGRES1-TOR-HOSPCD      PIC  X(10).
      *                    ���ŵ���̾
                           11  DRGRES1-TOR-HOSPNAME    PIC  X(80).
      *                    Ĵ�����ɥ�����
                           11  DRGRES1-TOR-CHOZAICD    PIC  X(10).
      *                    Ĵ������̾
                           11  DRGRES1-TOR-CHOZAINAME  PIC  X(80).
      *�ꥯ�������ֹ�ᣰ��
      *        �����Ͽǯ��
               05  DRGRES1-DATA02-AREA.
      *�ݸ�����
                   07  DRGRES1-HKNINF-G.
      *�ݸ����ֹ�
                       09  DRGRES1-HKNJANUM          PIC  X(08).
      *����
                       09  DRGRES1-KIGO              PIC  X(80).
      *�ֹ�
                       09  DRGRES1-NUM               PIC  X(80).
      *�ֹ����
                       09  DRGRES1-EDABAN            PIC  X(02).
      *
                   07  DRGRES1-MEISA-AREA.
                       09  DRGRES1-MEI-TBL           OCCURS   3.
      *            ����������
                       11  DRGRES1-NYUGAIKBN           PIC X(01).
      *
                       11  DRGRES1-DATE-REC.
      *Ĵ�����̾���
      *2022/08*****        13  DRGRES1-DI-DATE-OCC       OCCURS   31.
                           13  DRGRES1-DI-DATE-OCC       OCCURS   100.
      *Ĵ����
                               15  DRGRES1-DI-DAY        PIC X(02).
      *�����ȯ����
                               15  DRGRES1-PR-DATE       PIC X(10).
      *                    ���ŵ��إ�����
                               15  DRGRES1-HOSPCD        PIC  X(10).
      *                    ���ŵ���̾
                               15  DRGRES1-HOSPNAME      PIC  X(80).
      *                    Ĵ�����ɥ�����
                               15  DRGRES1-CHOZAICD      PIC  X(10).
      *                    Ĵ������̾
                               15  DRGRES1-CHOZAINAME    PIC  X(80).
      *���޾���
                               15  DRGRES1-PR-DATA-INFG.
      *���޾���
                                   16  DRGRES1-PR-DATA-INF
      *2022/08*****                                      OCCURS  100.
                                                         OCCURS  60.
      *���ż�������ʬ
                                       17  DRGRES1-SRYKBN   PIC X(02).
      *��ˡ������
                                       17  DRGRES1-YOHOCD   PIC X(03).
      *��ˡ̾��
                                       17  DRGRES1-YOHONAME PIC X(30).
      *���̻ؼ�
                                       17  DRGRES1-SHIJI    PIC X(80).
      *�����ʥ�����
                                       17  DRGRES1-SRYCD    PIC X(09).
      *����̾
                                       17  DRGRES1-YAKUZAINAME
                                                            PIC X(64).
      *��ʬ̾
                                       17  DRGRES1-SEIBUNNAME
                                                            PIC X(200).
      *ñ��
                                       17  DRGRES1-TANINAME PIC X(12).
      *������
                                       17  DRGRES1-SURYO    PIC X(11).
      *1������
                                       17  DRGRES1-YORYO    PIC X(11).
      *���
                                       17  DRGRES1-KAISU    PIC X(03).


