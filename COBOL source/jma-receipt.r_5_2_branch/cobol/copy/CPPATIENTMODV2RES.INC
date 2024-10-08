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
      *    ���Уɴ�����Ͽ����ʴ��ܾ����ݸ��������
      *                                   COPY (CPPATIENTMODV2RES.INC)
      ******************************************************************
        01  XML-PATIENTMODRES.
            02  PTMODRES-CONTEXT           PIC S9(9)   BINARY.
            02  PTMODRES-OBJECT            PIC X(36).
            02  PTMODRES-MODE              PIC S9(9)   BINARY.
            02  PTMODRES-RECORDNAME        PIC X(128).
            02  PTMODRES-APPOINTRES.
      *       �»���
              03  PTMODRES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  PTMODRES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  PTMODRES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  PTMODRES-API-RESULT-MSG      PIC X(100).
      *       ��˥󥰥�å�����
              03  PTMODRES-API-WARNING-MSG-G.
                  05  PTMODRES-API-WARNING-MSG     PIC X(100)
                                                   OCCURS   5.
      *
              03  PTMODRES-RESKEY              PIC X(100).
      *       �����ֹ�
              03  PTMODRES-PATIENTID           PIC X(20).
      *       ���Ծ���
              03  PTMODRES-PTINF-INFO.
      *         ���Ի�̾
                05  PTMODRES-NAME             PIC X(100).
      *         ���ԥ��ʻ�̾
                05  PTMODRES-KANANAME         PIC X(100).
      *         ��ǯ����
                05  PTMODRES-BIRTHDAY         PIC X(10).
      *         ����
                05  PTMODRES-SEX              PIC X(01).
      *         ���Ӽ�̾
                05  PTMODRES-SETAI-NAME       PIC X(100).
      *         ³��
                05  PTMODRES-ZOKUGARA         PIC X(60).
      *         ����
                05  PTMODRES-JOB              PIC X(40).
      *         ���������ֹ�
                05  PTMODRES-KEITAI-TEL       PIC  X(015).
      *         �ƣ����ֹ�
                05  PTMODRES-FAX              PIC  X(015).
      *         �Żҥ᡼�륢�ɥ쥹
                05  PTMODRES-EMAIL            PIC  X(050).
      *         �������
                05  PTMODRES-ADDRESS-INFO.
      *           ͹���ֹ�
                  07  PTMODRES-HOME-ZIP-CODE     PIC X(7).
      *           ����
                  07  PTMODRES-HOME-ADDRESSES1   PIC X(200).
      *           �����ֹ�
                  07  PTMODRES-HOME-ADDRESSES2   PIC X(200).
      *           �����ֹ�ʼ����
                  07  PTMODRES-HOME-HPONE1       PIC X(15).
      *           �����ֹ��Ϣ�����
                  07  PTMODRES-HOME-HPONE2       PIC X(15).
      *         ��̳�����
                05  PTMODRES-OFFICE-INFO.
      *           ��̳��̾
                  07  PTMODRES-OFFICE-NAME        PIC X(100).
      *           ͹���ֹ�
                  07  PTMODRES-OFFICE-ZIP-CODE    PIC X(7).
      *           ����
                  07  PTMODRES-OFFICE-ADDRESSES1  PIC X(200).
      *           �����ֹ�
                  07  PTMODRES-OFFICE-ADDRESSES2  PIC X(200).
      *           �����ֹ�
                  07  PTMODRES-OFFICE-HPONE        PIC X(15).
      *         �ش���
                05  PTMODRES-TABOO-1              PIC  X(200).
      *         �ش���
                05  PTMODRES-TABOO-2              PIC  X(200).
      *         ����륮����
                05  PTMODRES-ALLERGY-1            PIC  X(200).
      *         ����륮����
                05  PTMODRES-ALLERGY-2            PIC  X(200).
      *         �����ɣ�
                05  PTMODRES-KANSENSYO-1          PIC  X(200).
      *         �����ɣ�
                05  PTMODRES-KANSENSYO-2          PIC  X(200).
      *         �����ȣ�
                05  PTMODRES-COMMENT-1            PIC  X(200).
      *         �����ȣ�
                05  PTMODRES-COMMENT-2            PIC  X(200).
      *
      *         �ݸ�����
                05  PTMODRES-INSURANCE-INFO-G.
      *           �ݸ�������ݸ��ȹ礻��
                  06  PTMODRES-INSURANCE-INFO     OCCURS  20 TIMES.
      *               �ݸ��ȹ礻�ֹ�
                      07  PTMODRES-COMBINATION-NUMBER     PIC X(04).
      *               �ݸ��μ���
                      07  PTMODRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               �ݸ����ֹ�
                      07  PTMODRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *               �ݸ��μ���̾��
                      07  PTMODRES-MAIN-INSURANCE-NAME    PIC X(20).
      *               ����
                      07  PTMODRES-MAIN-MARK              PIC X(80).
      *               �ֹ�
                      07  PTMODRES-MAIN-NUMBER            PIC X(80).
      *R02.4          ����
                      07  PTMODRES-MAIN-EDABAN            PIC X(02).
      *               ��³��ʬ
                      07  PTMODRES-MAIN-CONTINUATION      PIC X(01).
      *               �����ʬ
                      07  PTMODRES-MAIN-ASSISTANCE        PIC X(01).
      *               �ܿͲ�²��ʬ
                      07  PTMODRES-MAIN-FAMILY            PIC X(01).
      *               ���ݸ���̾
                      07  PTMODRES-MAIN-POLICYHOLDER      PIC X(100).
      *               ͭ��������
                      07  PTMODRES-MAIN-START-DATE        PIC X(10).
      *               ͭ����λ��
                      07  PTMODRES-MAIN-END-DATE          PIC X(10).
      *               ��ʼ�����
                      07  PTMODRES-MAIN-GET-DATE          PIC X(10).
      *               �������
                      07  PTMODRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *                 ����μ���
                        09  PTMODRES-INSURANCE-CLASS         PIC X(03).
      *                 ����μ���̾��
                        09  PTMODRES-INSURANCE-NAME          PIC X(20).
      *                 ��ô���ֹ�
                        09  PTMODRES-PROVIDER-NUMBER         PIC X(08).
      *                 ������ֹ�
                        09  PTMODRES-RECIPIENT-NUMBER        PIC X(20).
      *                 ��������ôΨ�ʳ��
                        09  PTMODRES-RATE-ADMISSION          PIC X(04).
      *                 �����ݸ����
                        09  PTMODRES-MONEY-ADMISSION         PIC X(06).
      *                 �������ôΨ�ʳ��
                        09  PTMODRES-RATE-OUTPATIENT         PIC X(04).
      *                 ����ݸ����
                        09  PTMODRES-MONEY-OUTPATIENT        PIC X(06).
      *                 ͭ��������
                        09  PTMODRES-INSURANCE-START-DATE    PIC X(10).
      *                 ͭ����λ��
                        09  PTMODRES-INSURANCE-END-DATE      PIC X(10).
      *H30.4
      *         ���Ը��̾���
                05  PTMODRES-PTCONF-INFO.
      *             ǥ������
                    07  PTMODRES-NINPUKBN             PIC X(05).
      *R01.6
                05  PTMODRES-DICOUN-INFO.
      *             ���Ȼ�ͳ
                    07  PTMODRES-DISCOUNTKBN          PIC  X(02).
                    07  PTMODRES-DISCOUNTKBN-NAME     PIC  X(20).
      *             ���Ψ
                    07  PTMODRES-DISCOUNT             PIC  X(02).
                    07  PTMODRES-DISCOUNT-NAME        PIC  X(14).
      *             ��ʧ��ʬ��������ˡ��
                    07  PTMODRES-IC-CODE              PIC  X(02).
                    07  PTMODRES-IC-CODE-NAME         PIC  X(20).
      *R02.12
      *         ����
                05  PTMODRES-CONDITION-KBN-G.
                    07  PTMODRES-CONDITION-KBN-OCC    OCCURS   3.
                        09  PTMODRES-CONDITION-KBN    PIC  X(02).
                        09  PTMODRES-CONDITION-NAME   PIC  X(20).
      *
