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
      *----------------------------------------------------------*
      *    ����䵣ѣҥǡ������ϥѥ�᥿    COPY (CPORCSQRCSV.INC)
      *                                          (ORCHC02Q �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.06.00    NACL-¿��Ǽ  11/02/21  ��ˡ�������б�
      *  04.06.00    NACL-¿��Ǽ  12/03/01  ���̥������ɲá�H24.4��
      *  04.06.00    NACL-¿��Ǽ  12/09/27  ��ա��������ѥ��������ɲ�
      *  04.07.00    NACL-¿��Ǽ  15/01/21  �ãӣ֥ǡ���̾���ɲ�
      *  04.08.00    NACL-¿��Ǽ  18/03/XX  ���ͼ����ɲ�
      *  05.01.00    ORCAMO       22/09/XX  ������ֹ��ɲ�
      *  05.02.00    ORCAMO       23/04/XX  �����ٿ�50����100��
      ************************************************************
      *
       01  ORCSQRCSVAREA.
      *    1:���̾���2:�޾���3:��λ
           03  ORCSQRCSV-KBN               PIC X(01).
      *
      *    ���̾���
           03  ORCSQRCSV-IN01-G.
      *        ���ֹ�
               05  ORCSQRCSV-PREFNUM       PIC  9(02).
      *        ���ŵ��إ�����
               05  ORCSQRCSV-HOSPCD        PIC  9(07).
      *        ���ŵ��ؼ���
               05  ORCSQRCSV-HOSPSBT       PIC  9(01).
      *        ���ŵ���̾��
               05  ORCSQRCSV-HOSPNAME      PIC  X(120).
      *        ���ŵ���͹��
               05  ORCSQRCSV-HOSPPOST      PIC  X(08).
      *        ���ŵ��ؽ���
               05  ORCSQRCSV-HOSPADRS      PIC  X(100).
      *        ���ŵ��������ֹ�
               05  ORCSQRCSV-HOSPTEL       PIC  X(13).
      *        ���ŵ���FAX
               05  ORCSQRCSV-HOSPFAX       PIC  X(13).
      *        ���Ų�
               05  ORCSQRCSV-RECESRYKA     PIC  X(02).
               05  ORCSQRCSV-SRYKANAME     PIC  X(40).
      *        ��վ���
      *        �եꥬ��
               05  ORCSQRCSV-DR-KANANAME   PIC  X(40).
      *        ��̾
               05  ORCSQRCSV-DR-NAME       PIC  X(40).
      *        ��ե�����
               05  ORCSQRCSV-DR-CD         PIC  X(15).
      *        �������ѥ�����
               05  ORCSQRCSV-MAYAKULICENSE PIC  X(15).
      *        ���Ծ���
               05  ORCSQRCSV-PTNUM         PIC  X(15).
               05  ORCSQRCSV-NAME          PIC  X(40).
               05  ORCSQRCSV-KANANAME      PIC  X(40).
               05  ORCSQRCSV-SEX           PIC  X(01).
               05  ORCSQRCSV-BIRTHDAY      PIC  X(08).
               05  ORCSQRCSV-ADRS          PIC  X(100).
               05  ORCSQRCSV-TEL           PIC  X(13).
      *        �����ݸ�����
               05  ORCSQRCSV-HKNCOMBINUM   PIC  X(04).
      *        ���Ը���ɣ�
               05  ORCSQRCSV-KOHID-G       OCCURS   4.
                   07  ORCSQRCSV-KOHID         PIC 9(10).
                   07  ORCSQRCSV-KOHHKNNUM     PIC X(03).
      *R05.3
      *            �����������ֹ�
                   07  ORCSQRCSV-MONTH-JKYSNUM PIC X(20).
      *        ���ǥե饰
               05  ORCSQRCSV-MAYAKU        PIC  9(01).
      *        ������ô��ʬ
               05  ORCSQRCSV-BIKO-FTN      PIC  X(01).
      *        ������ǧ�ؼ�
               05  ORCSQRCSV-ZANSIJI       PIC  X(01).
      *H30.4
      *        ʬ��ؼ������
               05  ORCSQRCSV-BUNKATU-MAX   PIC  9(01).
      *        ʬ��ؼ�����
               05  ORCSQRCSV-BUNKATU-CNT   PIC  9(01).
      *R04.4
      *        �ե������
               05  ORCSQRCSV-REFILL-CNT    PIC  9(01).
      *R04.9
      *        ������ֹ�
               05  ORCSQRCSV-SYOHONUM      PIC  X(16).
      *
      *    ��������
           03  ORCSQRCSV-BIKOU-AREA.
               05  ORCSQRCSV-BIKOU-TBL       OCCURS   20.
                   07  ORCSQRCSV-BIKOU       PIC X(100).
      *            ���� ����
                   07  ORCSQRCSV-BIKOU-KBN   PIC X(02).
      *R06.3       ���͡�����ID
                   07  ORCSQRCSV-BIKOU-KOHID     PIC 9(10).
      *
      *    �ѣ��Ѻ��������
           03  ORCSQRCSV-NAIYOU-AREA.
               05  ORCSQRCSV-NAIYOU-TBL       OCCURS   50.
                   07  ORCSQRCSV-ZAIKATA          PIC X(01).
                   07  ORCSQRCSV-KAISU            PIC 9(03).
      *            ʬ������Ϳ����
                   07  ORCSQRCSV-GOKKAISU         PIC 9(03).
      *****        07  ORCSQRCSV-NAIYOU-TBL       OCCURS   50.
                   07  ORCSQRCSV-NAIYOU-TBL       OCCURS   100.
                       09  ORCSQRCSV-SRYCD            PIC X(09).
                       09  ORCSQRCSV-TNS-NAME         PIC X(100).
                       09  ORCSQRCSV-SURYOU           PIC 9(05)V9(05).
                       09  ORCSQRCSV-YKZKBN           PIC X(01).
                       09  ORCSQRCSV-TANINAME         PIC X(12).
                       09  ORCSQRCSV-KISAIKBN         PIC X(01).
                       09  ORCSQRCSV-KANZANCHI        PIC 9(05)V9(5).
      *                ��ˡ�����ȶ�ʬ
                       09  ORCSQRCSV-YCOMMNTKBN       PIC X(01).
      *                ����̾������
                       09  ORCSQRCSV-YAKKAKJNCD       PIC X(13).
      *H26.5
      *                ����������
                       09  ORCSQRCSV-SURYOU-SG        PIC 9(05)V9(05).
      *                �������Ѳ��
                       09  ORCSQRCSV-KAISU-SG         PIC 9(03).
      *H26.12
      *                CSV�ѡ�����̾��
                       09  ORCSQRCSV-CSV-NAME         PIC X(100).
      *
      *    �ե�����̾�Ȥ�Ω�ƹ���
           03  ORCSQRCSV-SYSTIME       PIC 9(08).
      *    ���顼������
           03  ORCSQRCSV-ERRCD         PIC 9(01).
      *    �꥿���󥳡���(�ѣҲ����С������)
           03  ORCSQRCSV-RET-VERSION        PIC 9(02).
      *    �꥿���󥳡���(�ѣҲ����ե������)
           03  ORCSQRCSV-RET-COUNT          PIC 9(02).
      *    qrencode return-code
           03  ORCSQRCSV-RET                PIC X(01).
      *H27.1
      *    �ãӣ֥ե�����̾��
           03  ORCSQRCSV-CSVFILE            PIC X(100).
      *
