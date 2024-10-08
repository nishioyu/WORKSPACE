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
      * 05.00.00     NACL-¿��Ǽ  20/04/XX  ���ݸ��Ծڻ����ɲ�
      *****************************************************************
      ******************************************************************
      *    ���Уɴ�����Ͽ����ʴ��ܾ����ݸ��������(xml2)
      *                                   COPY (CPPATIENTMODV2REQ.INC)
      ******************************************************************
        01  XML-PATIENTMODREQ.
            02  PTMODREQ-CONTEXT           PIC S9(9)   BINARY.
            02  PTMODREQ-OBJECT            PIC X(36).
            02  PTMODREQ-MODE              PIC S9(9)   BINARY.
            02  PTMODREQ-RECORDNAME        PIC X(128).
            02  PTMODREQ-MEDICALREQ.
      *       ��������
      *       03  PTMODREQ-INFORMATION-DATE    PIC X(10).
      *       03  PTMODREQ-INFORMATION-TIME    PIC X(8).
      *       ������ʬ
              03  PTMODREQ-MOD-KEY             PIC X(01).
      *       �����ֹ�
              03  PTMODREQ-PATIENTID           PIC X(20).
      *       ���Ծ���
              03  PTMODREQ-PTINF-INFO.
      *         ���Ի�̾
                05  PTMODREQ-NAME             PIC X(100).
      *         ���ԥ��ʻ�̾
                05  PTMODREQ-KANANAME         PIC X(100).
      *         ��ǯ����
                05  PTMODREQ-BIRTHDAY         PIC X(10).
      *         ����
                05  PTMODREQ-SEX              PIC X(01).
      *         ���Ӽ�̾
                05  PTMODREQ-SETAI-NAME       PIC X(100).
      *         ³��
                05  PTMODREQ-ZOKUGARA         PIC X(60).
      *         ����
                05  PTMODREQ-JOB              PIC X(40).
      *         ���������ֹ�
                05  PTMODREQ-KEITAI-TEL       PIC  X(015).
      *         �ƣ����ֹ�
                05  PTMODREQ-FAX              PIC  X(015).
      *         �Żҥ᡼�륢�ɥ쥹
                05  PTMODREQ-EMAIL            PIC  X(050).
      *         �������
                05  PTMODREQ-ADDRESS-INFO.
      *           ͹���ֹ�
                  07  PTMODREQ-HOME-ZIP-CODE     PIC X(7).
      *           ����
                  07  PTMODREQ-HOME-ADDRESSES1   PIC X(200).
      *           �����ֹ�
                  07  PTMODREQ-HOME-ADDRESSES2   PIC X(200).
      *           �����ֹ�ʼ����
                  07  PTMODREQ-HOME-HPONE1       PIC X(15).
      *           �����ֹ��Ϣ�����
                  07  PTMODREQ-HOME-HPONE2       PIC X(15).
      *         ��̳�����
                05  PTMODREQ-OFFICE-INFO.
      *           ��̳��̾
                  07  PTMODREQ-OFFICE-NAME        PIC X(100).
      *           ͹���ֹ�
                  07  PTMODREQ-OFFICE-ZIP-CODE    PIC X(7).
      *           ����
                  07  PTMODREQ-OFFICE-ADDRESSES1  PIC X(200).
      *           �����ֹ�
                  07  PTMODREQ-OFFICE-ADDRESSES2  PIC X(200).
      *           �����ֹ�
                  07  PTMODREQ-OFFICE-HPONE       PIC X(15).
      *         �ش���
                05  PTMODREQ-TABOO-1              PIC  X(200).
      *         �ش���
                05  PTMODREQ-TABOO-2              PIC  X(200).
      *         ����륮����
                05  PTMODREQ-ALLERGY-1            PIC  X(200).
      *         ����륮����
                05  PTMODREQ-ALLERGY-2            PIC  X(200).
      *         �����ɣ�
                05  PTMODREQ-KANSENSYO-1          PIC  X(200).
      *         �����ɣ�
                05  PTMODREQ-KANSENSYO-2          PIC  X(200).
      *         �����ȣ�
                05  PTMODREQ-COMMENT-1            PIC  X(200).
      *         �����ȣ�
                05  PTMODREQ-COMMENT-2            PIC  X(200).
      *
      *         �ݸ�����
                05  PTMODREQ-INSURANCE-INFO-G.
      *             �ݸ��μ���
                    07  PTMODREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *             �ݸ����ֹ�
                    07  PTMODREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *             �ݸ��μ���̾��
                    07  PTMODREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *             ����
                    07  PTMODREQ-MAIN-MARK               PIC X(80).
      *             �ֹ�
                    07  PTMODREQ-MAIN-NUMBER             PIC X(80).
      *R02.4        ����
                    07  PTMODREQ-MAIN-EDABAN             PIC X(02).
      *             ��³��ʬ
                    07  PTMODREQ-MAIN-CONTINUATION       PIC X(01).
      *             �����ʬ
                    07  PTMODREQ-MAIN-ASSISTANCE         PIC X(01).
      *             �ܿͲ�²��ʬ
                    07  PTMODREQ-MAIN-FAMILY             PIC X(01).
      *             �������ô��ʬ
                    07  PTMODREQ-RATE-CLASS              PIC X(01).
      *             ���ݸ���̾
                    07  PTMODREQ-MAIN-POLICYHOLDER       PIC X(100).
      *             ͭ��������
                    07  PTMODREQ-MAIN-START-DATE         PIC X(10).
      *             ͭ����λ��
                    07  PTMODREQ-MAIN-END-DATE           PIC X(10).
      *             ��ʼ�����
                    07  PTMODREQ-MAIN-GET-DATE           PIC X(10).
      *           �������
                  05  PTMODREQ-PUBLIC-INSURANCE-INF  OCCURS  4 TIMES.
      *             ����μ���
                    07  PTMODREQ-INSURANCE-CLASS       PIC X(03).
      *             ����μ���̾��
                    07  PTMODREQ-INSURANCE-NAME        PIC X(20).
      *             ��ô���ֹ�
                    07  PTMODREQ-PROVIDER-NUMBER       PIC X(08).
      *             ������ֹ�
                    07  PTMODREQ-RECIPIENT-NUMBER      PIC X(20).
      *             ͭ��������
                    07  PTMODREQ-INSURANCE-START-DATE  PIC X(10).
      *             ͭ����λ��
                    07  PTMODREQ-INSURANCE-END-DATE    PIC X(10).
      *H30.4
      *         ���Ը��̾���
                05  PTMODREQ-PTCONF-INFO.
      *             ǥ������
                    07  PTMODREQ-NINPUKBN             PIC X(05).
      *R01.6
                05  PTMODREQ-DICOUN-INFO.
      *             ���Ȼ�ͳ
                    07  PTMODREQ-DISCOUNTKBN          PIC  X(02).
      *             ���Ψ
                    07  PTMODREQ-DISCOUNT             PIC  X(02).
      *             ��ʧ��ʬ��������ˡ��
                    07  PTMODREQ-IC-CODE              PIC  X(02).
      *R02.12
      *         ����
                05  PTMODREQ-CONDITION-KBN-G.
                    07  PTMODREQ-CONDITION-KBN    PIC  X(02)
                                                  OCCURS   3.
