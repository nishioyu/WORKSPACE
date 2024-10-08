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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���Уɴ�����Ͽ����ʽ����Ծ���¾�����(���β���V3)
      *                                   COPY (CPPATIENTMODV3REQ4.INC)
      ******************************************************************
        01  XML-PATIENTMODREQ.
            02  PTMODREQ-CONTEXT           PIC S9(9)   BINARY.
            02  PTMODREQ-OBJECT            PIC X(36).
            02  PTMODREQ-MODE              PIC S9(9)   BINARY.
            02  PTMODREQ-RECORDNAME        PIC X(128).
            02  PTMODREQ-MEDICALREQ.
      *       �ꥯ�������ֹ�
              03  PTMODREQ-REQUEST-NUMBER   PIC X(2).
      *       UID���Żҥ���ơ�
              03  PTMODREQ-KARTE-UID        PIC X(36).
      *       UID�ʥ��륫��
              03  PTMODREQ-ORCA-UID         PIC X(36).
      *       ������աʥ����ƥ����ա�
              03  PTMODREQ-BASE-DATE        PIC X(10).
      *       ��ǧ��å���������
              03  PTMODREQ-SELECT-ANSWER    PIC X(03).
      *       ��³��ʬ
              03  PTMODREQ-CONT-MODE        PIC X(05).
      *       ���Ծ���
              03  PTMODREQ-PTINF-INFO.
      *         �����ֹ�
                05  PTMODREQ-PATIENTID        PIC X(20).
      *         ���Ի�̾
                05  PTMODREQ-NAME             PIC X(100).
      *         ���ԥ��ʻ�̾
                05  PTMODREQ-KANANAME         PIC X(100).
      *         ��ǯ����
                05  PTMODREQ-BIRTHDAY         PIC X(10).
      *         ����
                05  PTMODREQ-SEX              PIC X(01).
      *
      *       ������ԣ������������������
              03  PTMODREQ-TSYRRK-INFO-G.
      *          ������ʬ��MODIFY(NEW),DELETE)
                 05  PTMODREQ-TSYRRK-MODE        PIC X(06).
                 05  PTMODREQ-TSYRRK-INFO     OCCURS   20.
      *            ǧ�곫��ǯ����
                   07  PTMODREQ-TSYRRK-NINSTYMD    PIC X(10).
      *            ǧ�꽪λǯ����
                   07  PTMODREQ-TSYRRK-NINEDYMD    PIC X(10).
      *            ɸ����ô�۸��۳�����
                   07  PTMODREQ-TSYRRK-SKJNINSTYMD PIC X(10).
      *            Ĺ����������ǯ����
                   07  PTMODREQ-TSYRRK-SKJGNSTYMD  PIC X(10).
      *            ǧ���ϰ϶�ʬ
                   07  PTMODREQ-TSYRRK-HKNTEKKBN   PIC X(01).
      *H29.6       �����س�����ʬ
                   07  PTMODREQ-TSYRRK-KYOUKAISOU  PIC X(01).
      *
      *       ������ԣ������ǯ�������
              03  PTMODREQ-TNKRRK-INFO-G.
      *          ������ʬ
                 05  PTMODREQ-TNKRRK-MODE        PIC X(06).
                 05  PTMODREQ-TNKRRK-INFO        OCCURS   20.
      *            ǧ�곫��ǯ����
                   07  PTMODREQ-TNKRRK-STYMD       PIC X(10).
      *            ǧ�꽪λǯ����
                   07  PTMODREQ-TNKRRK-EDYMD       PIC X(10).
      *            ɸ����ô�۸��۳�����
                   07  PTMODREQ-TNKRRK-SKJNINSTYMD PIC X(10).
      *            Ϸ��ʡ��ǯ�����Ծڶ�ʬ
                   07  PTMODREQ-TNKRRK-FUKUSHI-KBN PIC X(01).
      *            ǧ���ϰ϶�ʬ
                   07  PTMODREQ-TNKRRK-HKNTEKKBN   PIC X(01).
      *H29.6       �����س�����ʬ
                   07  PTMODREQ-TNKRRK-KYOUKAISOU  PIC X(01).
      *
      *       ��������
              03  PTMODREQ-KYUSEIRRK-INFO-G.
      *           ������ʬ
                  05  PTMODREQ-KYUSEI-MODE        PIC X(06).
                  05  PTMODREQ-KYUSEIRRK-INFO     OCCURS   20.
      *            �ѹ�ǯ����
                   07  PTMODREQ-KYUSEI-CHGYMD      PIC X(10).
      *            �������ʻ�̾
                   07  PTMODREQ-KYUSEI-KANANAME    PIC X(100).
      *            ������̾
                   07  PTMODREQ-KYUSEI-NAME        PIC X(100).
      *            �����̾�̾
                   07  PTMODREQ-KYUSEI-NICKNAME    PIC X(100).
      *
      *       �õ�����
              03  PTMODREQ-PTTOKKI-INFO-G.
      *          ������ʬ
                 05  PTMODREQ-PTTOKKI-MODE        PIC X(06).
                 05  PTMODREQ-PTTOKKI-INFO     OCCURS   50.
      *            ������ʬ
                   07  PTMODREQ-PTTOKKI-NYUGAIKBN   PIC X(01).
      *            �쥻�Ŷ�ʬ
                   07  PTMODREQ-PTTOKKI-CD          PIC X(02).
      *            ��쥻�ץȡ��õ���ʬ���ơ�
                   07  PTMODREQ-PTTOKKI-NAIYO       PIC X(12).
      *            ����ǯ��
                   07  PTMODREQ-PTTOKKI-STYM        PIC X(07).
      *            ��λǯ��
                   07  PTMODREQ-PTTOKKI-EDYM        PIC X(07).
      *
      *       ���Ը��̾���
              03  PTMODREQ-PTCONF-INFO.
      *           ������ʬ
                  05  PTMODREQ-PTCONF-MODE        PIC X(06).
      *           �������ν�
                  05  PTMODREQ-BIRTHWEIGHT         PIC X(04).
      *           �ϰ��������оݼ���
                  05  PTMODREQ-CHKHKTBYO-G.
                      07  PTMODREQ-CHKHKTBYO       PIC X(05)
                                                   OCCURS   6.
                  05  PTMODREQ-CHKHKTBYO-G2    REDEFINES
                                               PTMODREQ-CHKHKTBYO-G.
      *               ��찵��
                      07  PTMODREQ-CHKHKTBYO-1      PIC X(05).
      *               ��Ǣ��
                      07  PTMODREQ-CHKHKTBYO-2      PIC X(05).
      *               ����۾��
                      07  PTMODREQ-CHKHKTBYO-3      PIC X(05).
      *               ǧ�ξ�
                      07  PTMODREQ-CHKHKTBYO-4      PIC X(05).
      *2022/04        ����������
                      07  PTMODREQ-CHKHKTBYO-5      PIC X(05).
      *2022/04        ������¡��
                      07  PTMODREQ-CHKHKTBYO-6      PIC X(05).
      *           ǧ�ξ��ϰ�������
                  05  PTMODREQ-NINTIHOKATU          PIC X(05).
      *           ����������Ĥ�����
                  05  PTMODREQ-SYOUNIKAKARI         PIC X(05).
      *H30.4      ǥ������
                  05  PTMODREQ-NINPUKBN             PIC X(05).
      *
