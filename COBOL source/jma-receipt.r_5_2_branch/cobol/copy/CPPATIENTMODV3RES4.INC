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
      *                                   COPY (CPPATIENTMODV3RES4.INC)
      ******************************************************************
       01  XML-PATIENTMODRES.
           02  PTMODRES-CONTEXT           PIC S9(9)   BINARY.
           02  PTMODRES-OBJECT            PIC X(36).
           02  PTMODRES-MODE              PIC S9(9)   BINARY.
           02  PTMODRES-RECORDNAME        PIC X(128).
           02  PTMODRES-APPOINTRES.
      *      �ꥯ�������ֹ�
             03  PTMODRES-REQUEST-NUMBER    PIC X(2).
      *      �쥹�ݥ��ֹ�
             03  PTMODRES-RESPONSE-NUMBER   PIC X(2).
      *      UID���Żҥ���ơ�
             03  PTMODRES-KARTE-UID         PIC X(36).
      *      UID�ʥ��륫��
             03  PTMODRES-ORCA-UID          PIC X(36).
      *      ��³��ʬ
             03  PTMODRES-CONT-MODE         PIC X(05).
      *      �»���
             03  PTMODRES-INFORMATION-DATE  PIC X(10).
      *      �»ܻ���
             03  PTMODRES-INFORMATION-TIME  PIC X(8).
      *      �ֵѥ�����
             03  PTMODRES-API-RESULT        PIC X(3).
      *      �ֵѥ�å�����
             03  PTMODRES-API-RESULT-MSG    PIC X(100).
      *
             03  PTMODRES-RESKEY              PIC X(100).
      *      �����ֹ�
             03  PTMODRES-PATIENTID        PIC X(20).
      *      ���Ծ���
             03  PTMODRES-PTINF-INFO.
      *        ���Ի�̾
               05  PTMODRES-NAME             PIC X(100).
      *        ���ԥ��ʻ�̾
               05  PTMODRES-KANANAME         PIC X(100).
      *        ��ǯ����
               05  PTMODRES-BIRTHDAY         PIC X(10).
      *        ����
               05  PTMODRES-SEX              PIC X(01).
      *
      *       ������ԣ������������������
              03  PTMODRES-TSYRRK-INFO-G.
                 05  PTMODRES-TSYRRK-INFO     OCCURS   20.
      *            ǧ�곫��ǯ����
                   07  PTMODRES-TSYRRK-NINSTYMD    PIC X(10).
      *            ǧ�꽪λǯ����
                   07  PTMODRES-TSYRRK-NINEDYMD    PIC X(10).
      *            ɸ����ô�۸��۳�����
                   07  PTMODRES-TSYRRK-SKJNINSTYMD PIC X(10).
      *            Ĺ����������ǯ����
                   07  PTMODRES-TSYRRK-SKJGNSTYMD  PIC X(10).
      *            ǧ���ϰ϶�ʬ
                   07  PTMODRES-TSYRRK-HKNTEKKBN   PIC X(01).
      *            ǧ���ϰ϶�ʬ̾��
                   07  PTMODRES-TSYRRK-HKNTEK-NAME PIC X(40).
      *H29.6       �����س�����ʬ
                   07  PTMODRES-TSYRRK-KYOUKAISOU   PIC X(01).
      *            �����س�����ʬ̾��
                   07  PTMODRES-TSYRRK-KYOUKAI-NAME PIC X(20).
      *            ���顼������
                   07  PTMODRES-TSYRRK-ERRCD       PIC X(04).
                   07  PTMODRES-TSYRRK-ERRCD-MSG   PIC X(100).
      *
      *       ������ԣ������ǯ�������
              03  PTMODRES-TNKRRK-INFO-G.
                 05  PTMODRES-TNKRRK-INFO        OCCURS   20.
      *            ǧ�곫��ǯ����
                   07  PTMODRES-TNKRRK-STYMD       PIC X(10).
      *            ǧ�꽪λǯ����
                   07  PTMODRES-TNKRRK-EDYMD       PIC X(10).
      *            ɸ����ô�۸��۳�����
                   07  PTMODRES-TNKRRK-SKJNINSTYMD PIC X(10).
      *            Ϸ��ʡ��ǯ�����Ծڶ�ʬ
                   07  PTMODRES-TNKRRK-FUKUSHI-KBN PIC X(01).
      *            ǧ���ϰ϶�ʬ
                   07  PTMODRES-TNKRRK-HKNTEKKBN   PIC X(01).
      *            ǧ���ϰ϶�ʬ̾��
                   07  PTMODRES-TNKRRK-HKNTEK-NAME PIC X(40).
      *H29.6       �����س�����ʬ
                   07  PTMODRES-TNKRRK-KYOUKAISOU   PIC X(01).
      *            �����س�����ʬ̾��
                   07  PTMODRES-TNKRRK-KYOUKAI-NAME PIC X(20).
      *            ���顼������
                   07  PTMODRES-TNKRRK-ERRCD       PIC X(04).
                   07  PTMODRES-TNKRRK-ERRCD-MSG   PIC X(100).
      *
      *       ��������
              03  PTMODRES-KYUSEIRRK-INFO-G.
                  05  PTMODRES-KYUSEIRRK-INFO     OCCURS   20.
      *            �ѹ�ǯ����
                   07  PTMODRES-KYUSEI-CHGYMD      PIC X(10).
      *            �������ʻ�̾
                   07  PTMODRES-KYUSEI-KANANAME    PIC X(100).
      *            ������̾
                   07  PTMODRES-KYUSEI-NAME        PIC X(100).
      *            �����̾�̾
                   07  PTMODRES-KYUSEI-NICKNAME    PIC X(100).
      *            ���顼������
                   07  PTMODRES-KYUSEI-ERRCD       PIC X(04).
                   07  PTMODRES-KYUSEI-ERRCD-MSG   PIC X(100).
      *
      *       �õ�����
              03  PTMODRES-PTTOKKI-INFO-G.
                 05  PTMODRES-PTTOKKI-INFO     OCCURS   50.
      *            ������ʬ
                   07  PTMODRES-PTTOKKI-NYUGAIKBN    PIC X(01).
                   07  PTMODRES-PTTOKKI-NYUGAI-NAME  PIC X(12).
      *            �쥻�Ŷ�ʬ
                   07  PTMODRES-PTTOKKI-CD          PIC X(02).
      *            ��쥻�ץȡ��õ���ʬ���ơ�
                   07  PTMODRES-PTTOKKI-NAIYO       PIC X(12).
      *            ����ǯ��
                   07  PTMODRES-PTTOKKI-STYM        PIC X(07).
      *            ��λǯ��
                   07  PTMODRES-PTTOKKI-EDYM        PIC X(07).
      *            ���顼������
                   07  PTMODRES-PTTOKKI-ERRCD       PIC X(04).
                   07  PTMODRES-PTTOKKI-ERRCD-MSG   PIC X(100).
      *
      *       ���Ը��̾���
              03  PTMODRES-PTCONF-INFO.
      *           �������ν�
                  05  PTMODRES-BIRTHWEIGHT         PIC X(04).
      *           �ϰ��������оݼ���
                  05  PTMODRES-CHKHKTBYO-G.
                      07  PTMODRES-CHKHKTBYO       PIC X(05)
                                                   OCCURS   6.
                  05  PTMODRES-CHKHKTBYO-G2    REDEFINES
                                               PTMODRES-CHKHKTBYO-G.
      *               ��찵��
                      07  PTMODRES-CHKHKTBYO-1      PIC X(05).
      *               ��Ǣ��
                      07  PTMODRES-CHKHKTBYO-2      PIC X(05).
      *               ����۾��
                      07  PTMODRES-CHKHKTBYO-3      PIC X(05).
      *               ǧ�ξ�
                      07  PTMODRES-CHKHKTBYO-4      PIC X(05).
      *2022/04        ����������
                      07  PTMODRES-CHKHKTBYO-5      PIC X(05).
      *2022/04        ������¡��
                      07  PTMODRES-CHKHKTBYO-6      PIC X(05).
      *           ǧ�ξ��ϰ�������
                  05  PTMODRES-NINTIHOKATU          PIC X(05).
      *           ����������Ĥ�����
                  05  PTMODRES-SYOUNIKAKARI         PIC X(05).
      *H30.4      ǥ������
                  05  PTMODRES-NINPUKBN             PIC X(05).
      *
      *      ���顼��å�����
             03  PTMODRES-PATIENT-ERRMSG-G.
                 05  PTMODRES-PATIENT-ERRMSG-INF  OCCURS   50.
                     07  PTMODRES-ERROR        PIC X(04).
                     07  PTMODRES-ERROR-MSG    PIC X(100).
      *
