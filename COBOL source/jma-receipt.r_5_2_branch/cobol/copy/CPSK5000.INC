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
      ************************************************************
      *         �����ƥ�����ޥ���     (CPSK5000.INC)   *
      *         �ʣ���������                            *
      *         ���ŵ��ؾ�����������ܾ���              *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.03.00    NACL-¿��Ǽ  08/08/28  �ౡ���������������ʬ�ɲ�
      *
      *  04.08.01    NACL-ƣ��    17/11/21  ����������ο�����ʬ�ɲ�
      ************************************************************
       01  SYS-5000-REC.
           03  SYS-5000-KEY.
               05  SYS-5000-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-5000-KANRICD        PIC X(04).
      *��ʬ������
               05  SYS-5000-KBNCD          PIC X(08).
      *ͭ������ǯ����
               05  SYS-5000-STYUKYMD       PIC X(08).
      *ͭ����λǯ����
               05  SYS-5000-EDYUKYMD       PIC X(08).
      *�����ơ��֥�
           03  SYS-5000-TBL.
      *��������������
               05  SYS-5000-SHOKUJIRYOYOCD
                                           PIC X(09).
      *��������
               05  SYS-5000-SHOKU-INF      OCCURS  5.
                   07  SYS-5000-SHOKUCD    PIC  X(09).
                   07  SYS-5000-SHOKUKBN   PIC  X(01).
      *
      *���ŵ��ظ�������
               05  SYS-5000-GEN-INF        OCCURS   5.
                   07  SYS-5000-GENCD      PIC  X(09).
                   07  SYS-5000-GENKBN     PIC  X(01).
      *�������û�����
               05  SYS-5000-KSN-INF        OCCURS  20.
                   07  SYS-5000-KSNCD      PIC  X(09).
                   07  SYS-5000-KSNKBN     PIC  X(01).
      *�ϰ�û�����
               05  SYS-5000-CHIIKIKSN      PIC  X(09).
      *������ץơ��֥�ɽ������
               05  SYS-5000-GENSAN-INF     OCCURS  10.
                   07  SYS-5000-DSPCD      PIC  X(09).
      *ͭ�����Ž�����¾�����û���ʬ
               05  SYS-5000-IPPANKSNKBN    PIC  X(01).
      *���ܴ����в����ֶ�ʬ
               05  SYS-5000-EIYOUKSNKBN    PIC  X(01).
      *�ߵޡ��������ٱ������¾�����û���ʬ
               05  SYS-5000-KYUKYUKSNKBN   PIC  X(01).
      *����������ο�����ʬ
               05  SYS-5000-SKJ-CREATEKBN  PIC  X(01).
      *ͽ���ʣ���
      ******   05                          PIC  X(60).
      ******   05                          PIC  X(56).
      ******   05  SYS-5000-YOBI           PIC  X(55).
      ******   05  SYS-5000-YOBI           PIC  X(54).
      ******   05  SYS-5000-YOBI           PIC  X(53).
      ******   05  SYS-5000-YOBI           PIC  X(52).
               05  SYS-5000-YOBI           PIC  X(51).
      *��ư��ɽ�����ʬ
               05  SYS-5000-IDORRK-SORTKBN PIC  X(01).
      *�������ο���
               05  SYS-5000-SKJKBN         PIC  X(01).
      *����������������
               05  SYS-5000-SENTEIKBN
                                           PIC  X(01).
      *�����ȯ����ˡ
               05  SYS-5000-SKYSUMKBN
                                           PIC  X(01).
      *�ౡ��������������ȯ��
               05  SYS-5000-SHOHOUKBN
                                           PIC  X(01).
      *������ɽ����ʬ
               05  SYS-5000-REKI-DISPKBN
                                           PIC  X(01).
      *�����ȯ�Զ�ʬ
               05  SYS-5000-SKYPRTKBN
                                           PIC  X(01).
      *�ౡ������ȯ�Զ�ʬ
               05  SYS-5000-TAIPRTKBN
                                           PIC  X(01).
      *���������ٽ�ȯ�Զ�ʬ
               05  SYS-5000-SRYPRTKBN
                                           PIC  X(01).
      *�ౡ�����񵭺ܶ�ʬ
               05  SYS-5000-TAIKSIKBN
                                           PIC  X(01).
      *�Ʒ׻���ʬ
               05  SYS-5000-SAIKEISANKBN   PIC  X(01).
      *�����̤���۰�����ʬ
               05  SYS-5000-SKYMISYUPRTKBN
                                           PIC  X(01).
      *����������ô��ʬ
               05  SYS-5000-SENTEI-FTNKBN
                                           PIC  X(01).
      *�������������Ƕ�ʬ
               05  SYS-5000-SENTEI-TAXKBN
                                           PIC  X(01).
      *������������ñ��
               05  SYS-5000-SENTEI-TANKA-X PIC  X(02).
               05  SYS-5000-SENTEI-TANKA-R REDEFINES
                   SYS-5000-SENTEI-TANKA-X.
                   07  SYS-5000-SENTEI-TANKA
                                           PIC  9(02).
      *����������������ʶ��̡�
               05  SYS-5000-SENTEI-KINGAKU-X
                                           PIC  X(04).
               05  SYS-5000-SENTEI-KINGAKU-R
                                           REDEFINES
                   SYS-5000-SENTEI-KINGAKU-X.
                   07  SYS-5000-SENTEI-KINGAKU
                                           PIC  9(04).
      *�������۾����Ƕ�ʬ
               05  SYS-5000-BRMSGK-TAXKBN
                                           PIC  X(01).
      *������׼������۶�ʬ
               05  SYS-5000-NACCT-BRMSGKKBN
                                           PIC  X(01).
      *����������������ʰ��̡�
               05  SYS-5000-SENTEI-IPN-KINGAKU-X
                                           PIC  X(04).
               05  SYS-5000-SENTEI-IPN-KINGAKU-R
                                           REDEFINES
                   SYS-5000-SENTEI-IPN-KINGAKU-X.
                   07  SYS-5000-SENTEI-IPN-KINGAKU
                                           PIC  9(04).
      *�����������������Ϸ�͡�
               05  SYS-5000-SENTEI-RJN-KINGAKU-X
                                           PIC  X(04).
               05  SYS-5000-SENTEI-RJN-KINGAKU-R
                                           REDEFINES
                   SYS-5000-SENTEI-RJN-KINGAKU-X.
                   07  SYS-5000-SENTEI-RJN-KINGAKU
                                           PIC  9(04).
      *������׳����ʬ
               05  SYS-5000-NACCT-GAIHAKUKBN
                                           PIC  X(01).
      *ɸ�縺�۶�ʬ
               05  SYS-5000-HYOUKETUKBN    PIC  X(01).
      *���Ķ���ʬ
               05  SYS-5000-TEISUKBN       PIC  X(01).
      *���ģ����϶�ʬ
               05  SYS-5000-ADLINPUTKBN    PIC  X(01).
      *������ẹ�۽��ɽ����ʬ
               05  SYS-5000-TEIKISAGAKUACTKBN
                                           PIC  X(01).
      *����μ谷��
               05  SYS-5000-NYKNKNRKBN     PIC  X(01).
      *����������¾��˲��ɽ����ʬ
               05  SYS-5000-RYOYODSPKBN    PIC  X(01).
      *ü���ɣ�
           03  SYS-5000-TERMID             PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-5000-OPID               PIC  X(16).
      *����ǯ����
           03  SYS-5000-CREYMD             PIC  X(08).
      *����ǯ����
           03  SYS-5000-UPYMD              PIC  X(08).
      *��������
           03  SYS-5000-UPHMS              PIC  X(06).
      *�祭�������ѹ���
           03  SYS-5000-UPD-KEY.
               05  SYS-5000-UPD-HOSPNUM     PIC 9(2).
               05  SYS-5000-UPD-KANRICD     PIC X(04).
               05  SYS-5000-UPD-KBNCD       PIC X(08).
               05  SYS-5000-UPD-STYUKYMD    PIC X(08).
               05  SYS-5000-UPD-EDYUKYMD    PIC X(08).
