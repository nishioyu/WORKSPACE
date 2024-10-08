      ******************************************************************
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
      ******************************************************************
      *    ��ʳ�ǧ����
      ******************************************************************
       01  ORCSONSHI001AREA.
      *    ���Ϲ���
           03  ORCSONSHI001-IN-AREA.
      *        ������ʬ    01:��ʳ�ǧ�����ֵ�
      *                    02:��ʳ�ǧ���ݰ���
      *                    03:��ʳ�ǧ�����ֵ�
      *                    04:�����ֹ��ѹ���������ʳ�ǧ
      *                    05:�ݸ��ڣϣãҺƾȲ�
               05  LNK-ONSHI001-SYOKBN PIC X(02).
      *        ��̳������  11:����
      *                    12:��Ͽ
      *                    21:���Ź԰�
      *                    99:����¾
               05  LNK-ONSHI001-GYOUMUCD
                                       PIC X(02).
      *        TBL_UUID
               05  LNK-ONSHI001-TBL-UUID
                                       PIC X(36).
      *        ����ID
               05  LNK-ONSHI001-PTID   PIC 9(10).
      *R04.1
      *        ���Ź԰ٻ�ʳ�ǧ̵ͭ
      *            0:̵����1:ͭ��
               05  LNK-ONSHI001-ONSHISINRYO  PIC X(01).
      *R04.9
      *    �ݸ��ڣϣãҺƾȲ�
      *        �ݸ����ֹ�
               05  LNK-ONSHI001-OCR-HKNJANUM     PIC X(08).
      *        ����
               05  LNK-ONSHI001-OCR-KIGO         PIC X(80).
      *        �ֹ�
               05  LNK-ONSHI001-OCR-NUM          PIC X(80).
      *        ����
               05  LNK-ONSHI001-OCR-EDABAN       PIC X(02).
      *        ��ǯ����
               05  LNK-ONSHI001-OCR-BIRTHDAY     PIC X(09).
      *R06.02
      *    �����ʿ��Ŷ�ʬ
               05  LNK-ONSHI001-ONSHINRYO-KBN    PIC X(01).
      *
      *    �ֵѹ���
           03  ORCSONSHI001-OUT-AREA.
      *        ��ʳ�ǧ����
               05  LNK-ONSHI001-KAKUNIN-FLG
                                       PIC X(03).
      *        ��ʾ���
               05  LNK-ONSHI001-SIKAKU-YUKO
                                       PIC X(01).
      *        ��ʾ�������
               05  LNK-ONSHI001-SIKAKU-YUKO-NM
                                       PIC X(100).
      *        UID
               05  LNK-ONSHI001-UID    PIC X(300).
      *        �ݸ��ѹ�����
               05  LNK-ONSHI001-HKN-FLG
                                       PIC X(03).
      *        ��ʳ�ǧ����(����:�Ӽ� etc)(���ա�������Ͽɽ������Ͽ�Ѿ���)
               05  LNK-ONSHI001-SHIKAKU
                                       PIC X(01). 
      *        �ޥ��ʥ�С�����
               05  LNK-ONSHI001-MYNUMBER
                                       PIC X(01).                                 
      *        ����
               05  LNK-ONSHI001-EDABAN PIC X(02).                                 
      *        TBL_UUID
               05  LNK-ONSHI001-OUT-TBL-UUID
                                       PIC X(36).
      *
      *���������� ������ʬ���顼
           03  LNK-ONSHI001-RC         PIC S9(9)   BINARY.
      *
