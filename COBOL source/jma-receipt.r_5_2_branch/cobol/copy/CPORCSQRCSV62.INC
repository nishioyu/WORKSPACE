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
      *    ������Ģ�ѣҥǡ������ϥѥ�᥿    COPY (CPORCSQRCSV62.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSQRCSV62AREA.
      *    Ģɼ̾
           03  ORCSQRCSV62-FORM-ID         PIC X(06).
      *    �������
           03  ORCSQRCSV62-CNT             PIC 9(03).
      *
      *    ���̾���
           03  ORCSQRCSV62-IN01-G.
      *        ����ǯ����
               05  ORCSQRCSV62-SRYYMD        PIC  X(08).
      *        ���ֹ�
               05  ORCSQRCSV62-PREFNUM       PIC  9(02).
      *        ���ŵ��إ�����
               05  ORCSQRCSV62-HOSPCD        PIC  9(07).
      *        ���ŵ��ؼ���
               05  ORCSQRCSV62-HOSPSBT       PIC  9(01).
      *        ���ŵ���̾��
               05  ORCSQRCSV62-HOSPNAME      PIC  X(120).
      *        ���ŵ���͹��
               05  ORCSQRCSV62-HOSPPOST      PIC  X(08).
      *        ���ŵ��ؽ���
               05  ORCSQRCSV62-HOSPADRS      PIC  X(800).
      *        ���ŵ��������ֹ�
               05  ORCSQRCSV62-HOSPTEL       PIC  X(13).
      *        ���Ų�
      *        05  ORCSQRCSV62-RECESRYKA     PIC  X(02).
      *        05  ORCSQRCSV62-SRYKANAME     PIC  X(40).
      *        ��վ���
      *        �ɥ�����������
               05  ORCSQRCSV62-DRCD          PIC  X(05).
      *        �եꥬ��
               05  ORCSQRCSV62-DR-KANANAME   PIC  X(40).
      *        ��̾
               05  ORCSQRCSV62-DR-NAME       PIC  X(40).
      *        ���Ծ���
               05  ORCSQRCSV62-NAME          PIC  X(40).
               05  ORCSQRCSV62-KANANAME      PIC  X(40).
               05  ORCSQRCSV62-SEX           PIC  X(02).
               05  ORCSQRCSV62-BIRTHDAY      PIC  X(08).
      *
      *    �ѣ��Ѻ��������
           03  ORCSQRCSV62-NAIYOU-AREA.
               05  ORCSQRCSV62-NAIYOU-TBL       OCCURS   50.
                   07  ORCSQRCSV62-SRYKBN           PIC X(02).
                   07  ORCSQRCSV62-SRYSYUKBN        PIC X(03).
                   07  ORCSQRCSV62-ZAIKATA          PIC X(01).
                   07  ORCSQRCSV62-KAISU            PIC 9(03).
                   07  ORCSQRCSV62-NAIYOU-TBL       OCCURS   100.
                       09  ORCSQRCSV62-SRYCD            PIC X(09).
      *R05.3           09  ORCSQRCSV62-TNS-NAME         PIC X(100).
                       09  ORCSQRCSV62-TNS-NAME         PIC X(120).
                       09  ORCSQRCSV62-SURYOU           PIC 9(05)V9(05).
                       09  ORCSQRCSV62-YKZKBN           PIC X(01).
                       09  ORCSQRCSV62-TANINAME         PIC X(12).
      *                �������޶�ʬ
                       09  ORCSQRCSV62-NAIHENKBN        PIC X(01).
      *                ��ˡ�����ȶ�ʬ
                       09  ORCSQRCSV62-YCOMMNTKBN       PIC X(01).
      *
      *    ��������
           03  ORCSQRCSV62-BIKOU-AREA.
               05  ORCSQRCSV62-BIKOU-TBL       OCCURS   10.
                   07  ORCSQRCSV62-BIKOU       PIC X(200).
      *
      *    �ե�����̾�Ȥ�Ω�ƹ���
           03  ORCSQRCSV62-SYSTIME         PIC 9(08).
      *
      *    �ѣңãӣ֥ե�����̾
           03  ORCSQRCSV62-CSV-FILENAME    PIC X(256).
      *    �ѣҽ��϶�ʬ
           03  ORCSQRCSV62-QR-KBN          PIC X(01).
      *    �ѣңãӣֽ�����ʬ
           03  ORCSQRCSV62-CSV-KBN         PIC X(01).
      *    ȯ�Բ��̶�ʬ
           03  ORCSQRCSV62-PGKBN           PIC X(01).
      *
      *����
      *    ���顼������
           03  ORCSQRCSV62-ERRCD           PIC 9(01).
      *
      *    �꥿���󥳡���(�ѣҲ����С������)
           03  ORCSQRCSV62-RET-VERSION     PIC 9(02).
      *    �꥿���󥳡���(�ѣҲ����ե������)
           03  ORCSQRCSV62-RET-COUNT       PIC 9(02).
      *    qrencode return-code
           03  ORCSQRCSV62-RET             PIC X(01).
      *
