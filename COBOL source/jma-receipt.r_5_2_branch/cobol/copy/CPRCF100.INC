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
      ******************************************************************
      *    �����쥻�ץȡ����ܾ���         COPY (CPRCF100.INC) 
      ******************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.01    NACL-����    10/02/16  ͽ�������ɲ�
      *
      *  04.06.01    NACL-����    10/07/02  ����������������󥻥����
      *                                     �ʣ����ܡ˻���ե饰�ɲ�
      *  04.08.01    NACL-����    18/04/06  ʿ������ǯ��������б�
      *  04.08.02    NACL-����    18/05/22  ʿ������ǯ��������б�
      *  04.08.03    NACL-����    18/06/11  �����������פΥ������б�
      *
      *  05.00.01    NACL-����    19/07/01  ��ϥӥ�ȯ�����쥻�ץȵ����б�
      *                                     �ʵ�ǽ�ɲá�
      *  05.00.02    NACL-����    20/04/07  ���£�ǯ��������б�
      *  05.00.03    NACL-����    20/07/01  ���£�ǯ��������б�
      *  05.00.04    NACL-����    20/11/27  �����б�
      ******************************************************************
      *01  RECE100-REC.
           03  RECE100-KEY.
      *���ŵ����ֹ�
               05  RECE100-HOSPNUM         PIC 9(2).
      *����ǯ��
               05  RECE100-SRYYM           PIC 9(06).
      *������ʬ
               05  RECE100-NYUGAIKBN       PIC X(01).
      *���ԣɣ�
               05  RECE100-PTID            PIC 9(10).
      *�쥻�ץȿ��Ų�
               05  RECE100-RECEKA          PIC X(02).
      *����ɣ�
               05  RECE100-HKNID           PIC 9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE100-TEKSTYMD        PIC X(08). 
      *
      *�쥻�ץȼ���
           03  RECE100-RECESYUBETU         PIC 9(04).
      *
      *�쥻�ץ��о�ǯ��������
           03  RECE100-TEKYMD.
      *Ŭ�ѳ���ǯ����
               05  RECE100-TEKSTYMD-X      PIC X(08). 
      *Ŭ�ѽ�λǯ����
               05  RECE100-TEKEDYMD-X      PIC X(08).
      *
      *���Ծ���
           03  RECE100-PTINF.
      *�����ֹ�  
               05  RECE100-PTNUM           PIC X(20).
      *���ʻ�̾
               05  RECE100-KANANAME        PIC X(50).
      *������̾
               05  RECE100-NAME            PIC X(100).
      *����
               05  RECE100-SEX             PIC X(01).
      *��ǯ����
               05  RECE100-BIRTHDAY        PIC 9(08).
      *��˴��ʬ
               05  RECE100-DEATHKBN        PIC X(01).
      *ǯ��
               05  RECE100-AGE             PIC 9(03).
      *�ƥ��ȴ����ֹ��ʬ
               05  RECE100-TSTPTNUMKBN     PIC X(01).
      *
      *���ݸ�����
           03  RECE100-HKNINF.
      *���ݸ����ݸ��ֹ�
               05  RECE100-HKNNUM          PIC X(03).
      *���ݸ����ݸ����ֹ�
               05  RECE100-HKNJANUM        PIC X(08).
      *���ݸ��ݵ���
               05  RECE100-KIGO            PIC X(80).
      *���ݸ����ֹ�
               05  RECE100-NUM             PIC X(80).
      *
      *���Ųʾ���
           03  RECE100-SRYKAINF            OCCURS  20.
      *���Ų�
               05  RECE100-SRYKA           PIC X(02).
      *
      *�ݸ��ȹ礻�ֹ�
           03  RECE100-HKNCOMBI            PIC 9(04).
      *
      *������
           03  RECE100-JNISSU              PIC 9(03).
      *
      *���Ŷ�ʬ����������ʣ��������ס�
           03  RECE100-SRYKBNINF           OCCURS  41.
      *��������
               05  RECE100-KHNTEN          PIC 9(08).
      *���
               05  RECE100-KAISU           PIC 9(05).
      *����
               05  RECE100-TENSU           PIC S9(08)   OCCURS  2.
      *
      *��׾���
           03  RECE100-TOTALTENINF-AREA.
               05  RECE100-TOTALTENINF     OCCURS  4.
      *�������
                   07  RECE100-TOTALTEN    PIC 9(08).
      *
      *�������ơ��֥����
           03  RECE100-JNISSUTBLINF        OCCURS  31.
      *�������ơ��֥�
               05  RECE100-JNISSUTBL       PIC X(01).
      *���Ļ����Ļ��Υ����ȥե饰
           03  RECE100-NYUYOJIFLG          PIC X(01).
      *����������������󥻥�����ʣ����ܡ˻���ե饰
           03  RECE100-SYOUNIFLG           PIC X(01).
      *������žɲ�ͽ�ɥ�������Ƴ���ʣ����ܤ���
      *�����ܤޤǡ˻���ե饰
           03  RECE100-NYUSENFLG           PIC X(01).
      *
      *��ǻ��ֶ�ʬ
           03  RECE100-SHOSHINTIME         PIC X(01).
      *�����®���θ����û��ե饰
           03  RECE100-KENTAIKASANFLG      PIC X(01).
      *�����ȥ�����ȥե饰
           03  RECE100-COMCNTFLG           PIC X(01).
      *������
           03  RECE100-PRTSEQ              PIC 9(03).
      *̤����ե饰
           03  RECE100-MISEIKYU-FLG        PIC X(02).
      *̤������ʬ
           03  RECE100-TEISEIKBN           PIC X(01).
      *�쥻�ץȼ����������顼����
           03  RECE100-ERR-AREA.
               05  RECE100-ERR-ERRKBN      PIC 9(01).
               05  RECE100-ERR-DAY         PIC 9(02).
               05  RECE100-ERR-HKNCOMBINUM PIC 9(04).
               05  RECE100-ERR-TEKSTYMD    PIC 9(08).
               05  RECE100-ERR-TEKEDYMD    PIC 9(08).
      *
      *�����������������ڤӱ���������������Ķ��ͳ�����Ⱦ���
           03  RECE100-SIPPUCHKFLG         PIC 9(01).
           03  RECE100-SIPPUCHKINF.
             04  RECE100-SIPPUCHK-OCC      OCCURS  10.
               05  RECE100-SIPPUCHK-OCC2   OCCURS  31.
                   07  RECE100-SIPPUCHK    PIC 9(03).
      *
      *�������������������ޤξ�硢���򥻥åȡ�
           03  RECE100-SIPPUCHK2FLG        PIC 9(01).
           03  RECE100-SIPPUCHK2INF.
             04  RECE100-SIPPUCHK2-OCC     OCCURS  10.
               05  RECE100-SIPPUCHK2-OCC2  OCCURS  31.
                   07  RECE100-SIPPUCHK2   PIC 9(01).
      *
      *ǥ�إե饰
           03  RECE100-NINPUFLG            PIC 9(01).
      *
      *�����ե饰
           03  RECE100-KENSA-BNPFLG        PIC 9(01).
           03  RECE100-KENSA-NTPROFLG      PIC 9(01).
           03  RECE100-KENSA-ANPFLG        PIC 9(01).
      *
      *��ϥӥ����
           03  RECE100-RIHAINF-AREA.
      *��ϥӥ껻��ơ��֥룲
               05  RECE100-RIHA2-DAY-AREA.
                 06  RECE100-RIHA2-SINDAI-AREA.
                   07  RECE100-RIHA2-SINDAI      PIC 9(01).
                   07  RECE100-RIHA2-SINYMD-OCC1.
                     08  RECE100-RIHA2-SINYMD1   PIC X(08)  OCCURS  2.
                   07  RECE100-RIHA2-SINYMD-OCC2.
                     08  RECE100-RIHA2-SINYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE100-RIHA2-KOKYUKI-AREA.
                   07  RECE100-RIHA2-KOKYUKI     PIC 9(01).
                   07  RECE100-RIHA2-KOKYMD-OCC1.
                     08  RECE100-RIHA2-KOKYMD1   PIC X(08)  OCCURS  2.
                   07  RECE100-RIHA2-KOKYMD-OCC2.
                     08  RECE100-RIHA2-KOKYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE100-RIHA2-NOUKE-AREA.
                   07  RECE100-RIHA2-NOUKE       PIC 9(01).
                   07  RECE100-RIHA2-NOUYMD-OCC1.
                     08  RECE100-RIHA2-NOUYMD1   PIC X(08)  OCCURS  2.
                   07  RECE100-RIHA2-NOUYMD-OCC2.
                     08  RECE100-RIHA2-NOUYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE100-RIHA2-UNDOKI-AREA.
                   07  RECE100-RIHA2-UNDOKI      PIC 9(01).
                   07  RECE100-RIHA2-UNDYMD-OCC1.
                     08  RECE100-RIHA2-UNDYMD1   PIC X(08)  OCCURS  2.
                   07  RECE100-RIHA2-UNDYMD-OCC2.
                     08  RECE100-RIHA2-UNDYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE100-RIHA2-NANBYO-AREA.
                   07  RECE100-RIHA2-NANBYO      PIC 9(01).
                   07  RECE100-RIHA2-NANYMD-OCC1.
                     08  RECE100-RIHA2-NANYMD1   PIC X(08)  OCCURS  2.
                   07  RECE100-RIHA2-NANYMD-OCC2.
                     08  RECE100-RIHA2-NANYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE100-RIHA2-SYOGAI-AREA.
                   07  RECE100-RIHA2-SYOGAI      PIC 9(01).
                   07  RECE100-RIHA2-SYOYMD-OCC1.
                     08  RECE100-RIHA2-SYOYMD1   PIC X(08)  OCCURS  2.
                   07  RECE100-RIHA2-SYOYMD-OCC2.
                     08  RECE100-RIHA2-SYOYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE100-RIHA2-SESSYOKU-AREA.
                   07  RECE100-RIHA2-SESSYOKU    PIC 9(01).
                   07  RECE100-RIHA2-SESYMD-OCC1.
                     08  RECE100-RIHA2-SESYMD1   PIC X(08)  OCCURS  2.
                   07  RECE100-RIHA2-SESYMD-OCC2.
                     08  RECE100-RIHA2-SESYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE100-RIHA2-HAIYO-AREA.
                   07  RECE100-RIHA2-HAIYO       PIC 9(01).
                   07  RECE100-RIHA2-HAIYMD-OCC1.
                     08  RECE100-RIHA2-HAIYMD1   PIC X(08)  OCCURS  2.
                   07  RECE100-RIHA2-HAIYMD-OCC2.
                     08  RECE100-RIHA2-HAIYMD2   PIC 9(01)  OCCURS 31.
      *��ϥӥ�û�����ơ��֥룲
               05  RECE100-RIHA2-DAY9-AREA.
                 06  RECE100-RIHA2-SINDAI9-AREA.
                   07  RECE100-RIHA2-SINDAI9     PIC 9(01).
                   07  RECE100-RIHA2-SIN9YMD-OCC1.
                     08  RECE100-RIHA2-SIN9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE100-RIHA2-SIN9YMD-OCC2.
                     08  RECE100-RIHA2-SIN9YMD2  PIC 9(01)  OCCURS 31.
      *
                 06  RECE100-RIHA2-KOKYUKI9-AREA.
                   07  RECE100-RIHA2-KOKYUKI9    PIC 9(01).
                   07  RECE100-RIHA2-KOK9YMD-OCC1.
                     08  RECE100-RIHA2-KOK9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE100-RIHA2-KOK9YMD-OCC2.
                     08  RECE100-RIHA2-KOK9YMD2  PIC 9(01)  OCCURS 31.
      *
                 06  RECE100-RIHA2-HAIYO9-AREA.
                   07  RECE100-RIHA2-HAIYO9      PIC 9(01).
                   07  RECE100-RIHA2-HAI9YMD-OCC1.
                     08  RECE100-RIHA2-HAI9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE100-RIHA2-HAI9YMD-OCC2.
                     08  RECE100-RIHA2-HAI9YMD2  PIC 9(01)  OCCURS 31.
      *
      *��ϥӥ�û�����
           03  RECE100-RIHAKSNINF-AREA.
               05  RECE100-RSKSN-SYOKI-SIN       PIC 9(01).
               05  RECE100-RSKSN-SOUKI-SIN       PIC 9(01).
               05  RECE100-RSKSN-SYOKI-KOK       PIC 9(01).
               05  RECE100-RSKSN-SOUKI-KOK       PIC 9(01).
               05  RECE100-RSKSN-SYOKI-HAI       PIC 9(01).
               05  RECE100-RSKSN-SOUKI-HAI       PIC 9(01).
      *
      *������Ǣ��Ω��Ƴ���γ�����ǽ�λ�����
           03  RECE100-GAIJIRYMD           PIC X(08).
      *
      *ͽ��
           03  RECE100-YOBIETC-AREA.
               05  RECE100-YOBIETCINF      OCCURS 10.
                   07  RECE100-YOBIETC     PIC 9(09).
      *
