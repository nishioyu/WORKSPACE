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
      *    �Żҽ���䵥ǡ������ϥѥ�᥿    COPY (CPORCSEPRECSV.INC)
      *                                          (ORCHC02Q �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSEPRECSVAREA.
      *    API:CSV�ơ��֥��ֵ�
           03  ORCSEPRECSV-SYORIKBN          PIC X(03).
      *
      *    ���̾���
           03  ORCSEPRECSV-IN01-G.
      *        ���ֹ�
               05  ORCSEPRECSV-PREFNUM       PIC  9(02).
      *        ���ŵ��إ�����
               05  ORCSEPRECSV-HOSPCD        PIC  9(07).
      *        ���ŵ��ؼ���
               05  ORCSEPRECSV-HOSPSBT       PIC  9(01).
      *        ���ŵ���̾��
               05  ORCSEPRECSV-HOSPNAME      PIC  X(180).
      *        ���ŵ���͹��
               05  ORCSEPRECSV-HOSPPOST      PIC  X(08).
      *        ���ŵ��ؽ���
               05  ORCSEPRECSV-HOSPADRS      PIC  X(150).
      *        ���ŵ��������ֹ�
               05  ORCSEPRECSV-HOSPTEL       PIC  X(13).
      *        ���ŵ���FAX
               05  ORCSEPRECSV-HOSPFAX       PIC  X(13).
      *        ���Ų�
               05  ORCSEPRECSV-RECESRYKA     PIC  X(02).
               05  ORCSEPRECSV-SRYKANAME     PIC  X(120).
      *        ��վ���
      *        �եꥬ��
               05  ORCSEPRECSV-DR-KANANAME   PIC  X(60).
      *        ��̾
               05  ORCSEPRECSV-DR-NAME       PIC  X(60).
      *        ��ե�����
               05  ORCSEPRECSV-DR-CD         PIC  X(15).
      *        �������ѥ�����
               05  ORCSEPRECSV-MAYAKULICENSE PIC  X(60).
      *        ���Ծ���
               05  ORCSEPRECSV-PTNUM         PIC  X(15).
               05  ORCSEPRECSV-NAME          PIC  X(60).
               05  ORCSEPRECSV-KANANAME      PIC  X(60).
               05  ORCSEPRECSV-SEX           PIC  X(01).
               05  ORCSEPRECSV-BIRTHDAY      PIC  X(08).
               05  ORCSEPRECSV-ADRS          PIC  X(150).
               05  ORCSEPRECSV-TEL           PIC  X(13).
      *        �����ݸ�����
               05  ORCSEPRECSV-HKNCOMBINUM   PIC  X(04).
      *        ���Ը���ɣ�
               05  ORCSEPRECSV-KOHID-G       OCCURS   4.
                   07  ORCSEPRECSV-KOHID         PIC 9(10).
                   07  ORCSEPRECSV-KOHHKNNUM     PIC X(03).
      *R05.3
      *            �����������ֹ�
                   07  ORCSEPRECSV-MONTH-JKYSNUM PIC X(20).
      *        ���ǥե饰
               05  ORCSEPRECSV-MAYAKU        PIC  9(01).
      *        ������ô��ʬ
               05  ORCSEPRECSV-BIKO-FTN      PIC  X(01).
      *        ������ǧ�ؼ�
               05  ORCSEPRECSV-ZANSIJI       PIC  X(01).
      ****
      *        ʬ��ؼ������
      *         05  ORCSEPRECSV-BUNKATU-MAX   PIC  9(01).
      *        ʬ��ؼ�����
      *         05  ORCSEPRECSV-BUNKATU-CNT   PIC  9(01).
      *??
      *        �ե������
                05  ORCSEPRECSV-REFILL-CNT    PIC  9(01).
      *
      *    ��������
           03  ORCSEPRECSV-BIKOU-AREA.
               05  ORCSEPRECSV-BIKOU-TBL       OCCURS   20.
                   07  ORCSEPRECSV-BIKOU       PIC X(100).
      *            ���� ����
                   07  ORCSEPRECSV-BIKOU-KBN   PIC X(02).
      *2024/03
      *            ���� ����ID
                   07  ORCSEPRECSV-BIKOU-KOHID PIC 9(10).
      *
      *    ���������
           03  ORCSEPRECSV-NAIYOU-AREA.
               05  ORCSEPRECSV-NAIYOU-TBL       OCCURS   50.
                   07  ORCSEPRECSV-ZAIKATA          PIC X(01).
                   07  ORCSEPRECSV-KAISU            PIC 9(03).
      *            ʬ������Ϳ����
                   07  ORCSEPRECSV-GOKKAISU         PIC 9(03).
      *2023/04     07  ORCSEPRECSV-NAIYOU-TBL       OCCURS   50.
                   07  ORCSEPRECSV-NAIYOU-TBL       OCCURS   100.
                       09  ORCSEPRECSV-SRYCD            PIC X(09).
                       09  ORCSEPRECSV-TNS-NAME         PIC X(180).
                       09  ORCSEPRECSV-SURYOU           PIC 9(05)V9(05).
                       09  ORCSEPRECSV-YKZKBN           PIC X(01).
                       09  ORCSEPRECSV-TANINAME         PIC X(12).
                       09  ORCSEPRECSV-KISAIKBN         PIC X(01).
                       09  ORCSEPRECSV-KANZANCHI        PIC 9(05)V9(5).
      *                ��ˡ�����ȶ�ʬ
                       09  ORCSEPRECSV-YCOMMNTKBN       PIC X(01).
      *                ����̾������
                       09  ORCSEPRECSV-YAKKAKJNCD       PIC X(13).
      *                ����̾�Ρʾ�ά�Բġ�
                       09  ORCSEPRECSV-CSV-NAME         PIC X(180).
      *                ����̾
                       09  ORCSEPRECSV-SHO-NAME         PIC X(180).
      *                ����������
                       09  ORCSEPRECSV-SURYOU-SG        PIC 9(05)V9(05).
      *                �������Ѳ��
                       09  ORCSEPRECSV-KAISU-SG         PIC 9(03).
      *
      *    ����䵾���
           03  ORCSEPRECSV-PRESCRIPTION-INFO.
      *        �����ȯ�Է���
               05  ORCSEPRECSV-PRE-ISSUESELECT   PIC X(1).
      *        �����ID
               05  ORCSEPRECSV-PRE-ID            PIC X(36).
      *        �����ֹ�
               05  ORCSEPRECSV-ACCESS-CODE       PIC X(16).
      *        ����䵥����Ⱦ���
               05  ORCSEPRECSV-COMMENT-G.
                   07   ORCSEPRECSV-COMMENT-OCC   OCCURS  10.
                        09   ORCSEPRECSV-PRE-NAME     PIC X(180).
                        09   ORCSEPRECSV-PRE-COMMENT  PIC X(300).
      *
      *    �ե�����̾�Ȥ�Ω�ƹ���
           03  ORCSEPRECSV-SYSTIME       PIC 9(08).
      *    ���顼������
           03  ORCSEPRECSV-ERRCD         PIC 9(01).
      *    qrencode return-code
      *     03  ORCSEPRECSV-RET                PIC X(01).
      *    �ãӣ֥ե�����̾��
           03  ORCSEPRECSV-CSVFILE            PIC X(1024).
      *
      *    API�λ��ãӣ֥ǡ������ֵ�
           03  ORCSEPRECSV-CSVDATA-AREA.
               05  ORCSEPRECSV-CSVDATA-OCC       OCCURS   500.
                   07  ORCSEPRECSV-CSVDATA       PIC X(500).
               05  ORCSEPRECSV-CSVMAX            PIC 9(04).
      *
      *
