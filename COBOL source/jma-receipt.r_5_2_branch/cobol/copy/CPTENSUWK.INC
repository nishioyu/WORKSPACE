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
      *         �����ޥ��������ѥե�����    COPY (CPTENSUUWK.INC)*
      *                       RECORD LEN  658 BYTE               *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     MCC-¿��Ǽ   02/03/18  ��ȯ�����ʶ�ʬ��
      *                                     �����Ŭ��������ʬ
      *                                     �����Ŭ���������� �ɲ�
      * 01.00.02     NACL-¿��Ǽ  04/12/20  �������������ɲ�
      * 01.00.03     NACL-��ƣ    06/03/15  �ɥʡ�ʬ���׶�ʬ �ɲ�
      * 01.00.04     NACL-��ƣ    08/03/07  ������ѻٱ�û�
      *                                     ���Ŵѻ�ˡ�оݶ�ʬ
      *                                     ��켱�̶�ʬ �ɲ�
      * 04.06.00     NACL-¿��Ǽ  10/10/29  ���ߴ�ࡡ�����б�
      * 04.07.00     NACL-��ƣ    14/03/06  ��ɡ�м���������û�
      *                                     ��ɡ�м���ѹ������ȿ��ڽ�����û�
      *                                     Ĺ�����������û�
      *                                     DPCŬ����ʬ
      * 04.08.00     NACL-¿��Ǽ  16/03/XX  �󿯽�Ū���ư�֥�˥���󥰲û�
      *                                     �����¸Ʊ���ȿ��û�
      *                                     ����ɽ��ʬ�ֹ�
      *                                     ϫ�һ����Բ�
      *                                     �ͻ�û���ϫ�ҡ�
      * 05.01.00     NACL-��ƣ    18/03/05  �������������ȿ�ɸ�ܲû�
      * 05.02.00     NMED-��ƣ    20/03/06  �ϳ������û�
      *                                     Ķ�����ں��û�
      *                                     �������ĺ���ʻ�ܶ�ʬ
      * 05.03.00     ORCAMO       22/03/03  ���贶�����к�����û���
      *                                     ��ɡ���������Ļ����ֲû�
      *                                     ��ɡ�����ʾ���������Ŭ�����ѻٱ�û�
      *                                     �ڳ��϶ɽ걢���ĺ���������û�
      * 05.03.00     ORCAMO       24/04/15  �Ǹ���������ɾ������
      *                                     ���衦����١������å�ɾ�����ʣ���
      *                                     ���衦����١������å�ɾ�����ʣ���
      *                                     ����¤ñ����Ѱ��ŵ�����Ѳû�
      *                                     ����������ǯ����
      *                                     ����̾������
      *                                     ����̾�����û��оݶ�ʬ
      *                                     ���ȣɣ�����ʬ
      ************************************************************
       01  TNSWK-TENSU-REC.
      *(1)
           03  TNSWK-SYORIKBN            PIC X(01).
           03  TNSWK-KEY.
               05  TNSWK-SRYCD           PIC  X(09).
               05  TNSWK-YUKOSTYMD       PIC  X(08).
               05  TNSWK-YUKOEDYMD       PIC  X(08).
           03  TNSWK-YUKOEDYMD2          PIC  X(08).
           03  TNSWK-SRYKBN              PIC  X(02).
           03  TNSWK-SRYSYUKBN           PIC  X(03).
           03  TNSWK-YUKOKETA            PIC  9(02).
           03  TNSWK-NAME                PIC  X(200).
           03  TNSWK-KANAYUKOKETA        PIC  9(02).
           03  TNSWK-KANANAME            PIC  X(200).
           03  TNSWK-FORMALYUKOKETA      PIC  9(03).
           03  TNSWK-FORMALNAME          PIC  X(400).
           03  TNSWK-TENSIKIBETU         PIC  9(01).
           03  TNSWK-TEN                 PIC  9(09)V99.
           03  TNSWK-TANICD              PIC  X(03).
           03  TNSWK-TANIMOJISU          PIC  9(01).
           03  TNSWK-TANINAME            PIC  X(24).
           03  TNSWK-DATAKBN             PIC  9(01).
           03  TNSWK-HKNTEKKBN           PIC  9(01).
           03  TNSWK-NYUGAITEKKBN        PIC  9(01).
      *(20)
           03  TNSWK-ROUTEKKBN           PIC  9(01).
           03  TNSWK-HOSPSRYKBN          PIC  9(01).
           03  TNSWK-OSINKBN             PIC  9(01).
           03  TNSWK-HOUKNSKBN           PIC  9(02).
           03  TNSWK-BYOKANRENKBN        PIC  9(02).
           03  TNSWK-SDOKNRYO            PIC  9(02).
           03  TNSWK-JITUDAY             PIC  9(01).
           03  TNSWK-DAYCNT              PIC  9(01).
           03  TNSWK-YKZKNRKBN           PIC  9(01).
           03  TNSWK-KZMCOMPSIKIBETU     PIC  9(01).
           03  TNSWK-KZMKGN              PIC  9(08).
           03  TNSWK-KZMJGN              PIC  9(08).
           03  TNSWK-KZM                 PIC  9(08).
           03  TNSWK-KZMTEN              PIC  9(07)V99.
           03  TNSWK-KZMERR              PIC  9(01).
           03  TNSWK-JGNCNT              PIC  9(03).
           03  TNSWK-JGNCNT1D            PIC  9(03).
           03  TNSWK-JGNCNT1W            PIC  9(03).
           03  TNSWK-JGNCNTETCM          PIC  9(02).
           03  TNSWK-JGNCNTETC           PIC  9(03).
      *(40)
           03  TNSWK-JGNCNTERR           PIC  9(01).
           03  TNSWK-CHUKSNCD            PIC  X(04).
           03  TNSWK-CHUKSNTUBAN         PIC  X(01).
           03  TNSWK-TSUSOKUAGE          PIC  9(01).
           03  TNSWK-KGNAGE              PIC  X(02).
           03  TNSWK-JGNAGE              PIC  X(02).
           03  TNSWK-TIMEKSNKBN          PIC  9(01).
      *    �����Ŭ��������ʬ
           03  TNSWK-KIJUNTEIGENKBN      PIC  9(01).
      *    �����Ŭ�������оݻ��ߴ�ॳ����
           03  TNSWK-KIJUNTEIGENCD       PIC  9(04).
           03  TNSWK-LASERKBN            PIC  9(01).
           03  TNSWK-CHPMESUKSN          PIC  9(01).
           03  TNSWK-MICMESUKSN          PIC  9(01).
           03  TNSWK-DONORKBN            PIC  9(01).
           03  TNSWK-KNSJISKBN           PIC  9(01).
           03  TNSWK-KNSJISGRPKBN        PIC  9(02).
           03  TNSWK-TEIGENKBN           PIC  9(01).
           03  TNSWK-SEKIZUISOKUTEI      PIC  9(01).
           03  TNSWK-KEIBUJYUTU          PIC  9(01).
           03  TNSWK-AUTOHOUGO           PIC  9(01).
           03  TNSWK-GAIKANRIKBN         PIC  9(01).
           03  TNSWK-TUSOKUGAIKBN        PIC  9(01).
           03  TNSWK-HOUKATUTEIGENKBN    PIC  9(03).
           03  TNSWK-CHPNAISIKSN         PIC  9(01).
      *(60)
           03  TNSWK-AUTOFUNGO           PIC  9(01).
           03  TNSWK-CHIIKIKSN           PIC  9(01).
           03  TNSWK-BYOSYOKBN           PIC  9(01).
           03  TNSWK-CHPGYOKOSOTIKSN     PIC  9(01).
           03  TNSWK-SHORTSTAYOPE        PIC  9(01).
           03  TNSWK-BUIKBN              PIC  9(01).
           03  TNSWK-SANTEIRRKKBN        PIC  9(01).
           03  TNSWK-SSTKIJUNCDTBL       OCCURS 10.
               05  TNSWK-SSTKIJUNCD      PIC  9(04).
           03  TNSWK-AGEKSNTBL           OCCURS 4.
               05  TNSWK-AGEKSNKGN       PIC  X(02).
               05  TNSWK-AGEKSNJGN       PIC  X(02).
               05  TNSWK-AGEKSNSRYCD     PIC  X(09).
           03  TNSWK-KENTAICOMMENT       PIC  9(01).
      *(90)
           03  TNSWK-NYUKHNKBN           PIC  9(02).
           03  TNSWK-NYUKHNKSNKBN        PIC  9(03).
           03  TNSWK-KANGOKSN            PIC  9(02).
           03  TNSWK-GAZOOPESUP          PIC  9(01).
           03  TNSWK-IRYOKANSATUKBN      PIC  9(01).
           03  TNSWK-MASUISKBKBN         PIC  9(01).
      *H26.4 ADD START
           03  TNSWK-FUKUBIKUNAIKBN      PIC  9(01).
           03  TNSWK-FUKUBIKUKOTUKBN     PIC  9(01).
           03  TNSWK-TIMEMASUIKBN        PIC  9(01).
           03  TNSWK-DPCKBN              PIC  9(01).
      *H26.4 ADD END
      *H28.4 ADD START
           03  TNSWK-HISINSYUMONITORKSN  PIC  9(01).
           03  TNSWK-TOUKETUHOZONKSN     PIC  9(01).
           03  TNSWK-KUBUNBANGOU         PIC  X(30).
           03  TNSWK-ROSAIKBN            PIC  9(01).
           03  TNSWK-SISIKSN             PIC  9(01).
      *H28.4 ADD END
      *H30.4 ADD START
           03  TNSWK-AKUSEIBYORIKSN      PIC  9(01).
      *H30.4 ADD END
      *R02.4 ADD START
      *    �ϳ������û�
           03  TNSWK-SOGAIKOTEIKSN       PIC  9(01).
      *    Ķ�����ں��û�
           03  TNSWK-CHPSESSAKUKSN       PIC  9(01).
      *    �������ĺ���ʻ�ܶ�ʬ
           03  TNSWK-SASINJIHEISIKBN     PIC  9(01).
      *R02.4 ADD END
      *R04.4 ADD START
      *    ���贶�����к�����û���
      *R06.4 CHG
      *    03  TNSWK-GAIKANSENSYOKSN     PIC  9(01).
           03  TNSWK-GAIKANSENSYOKSN     PIC  X(01).
      *    ��ɡ���������Ļ����ֲû�
           03  TNSWK-JIBINYUYOJIKSN      PIC  9(01).
      *    ��ɡ�����ʾ���������Ŭ�����ѻٱ�û�
           03  TNSWK-JIBISYONIKOKINKSN   PIC  9(01).
      *    �ڳ��϶ɽ걢���ĺ���������û�
           03  TNSWK-SEKKAISOUKIKIKSN    PIC  9(01).
      *R04.4 ADD END
      *R06.4 ADD START
      *    �Ǹ���������ɾ������
           03  TNSWK-KANGOHYOKA          PIC  X(01).
      *    ���衦����١������å�ɾ�����ʣ���
           03  TNSWK-BASEUPHYOKA1        PIC  X(01).
      *    ���衦����١������å�ɾ�����ʣ���
           03  TNSWK-BASEUPHYOKA2        PIC  X(01).
      *    ����¤ñ����Ѱ��ŵ�����Ѳû�
           03  TNSWK-RSUDKSN             PIC  9(03).
      *R06.4 ADD END
           03  TNSWK-OLDTENSKB           PIC  9(01).
           03  TNSWK-OLDTEN              PIC  9(09)V99.
           03  TNSWK-MADOKUKBN           PIC  9(01).
           03  TNSWK-SINKEIHAKAIKBN      PIC  9(01).
           03  TNSWK-SEIBUTUGAKUKBN      PIC  9(01).
           03  TNSWK-ZOEIZAIKBN          PIC  9(01).
           03  TNSWK-CSYYOURYO           PIC  9(05).
      *(100)
           03  TNSWK-YKZKBN              PIC  9(01).
           03  TNSWK-ZAIGATAKBN          PIC  9(01).
      *    ��ȯ�����ʶ�ʬ
           03  TNSWK-KOUHATUKBN          PIC  9(01).
           03  TNSWK-LONGTOYOKBN         PIC  9(01).
           03  TNSWK-MEIUSESKB           PIC  9(01).
           03  TNSWK-TOKUKIZAIAGEKSNKBN  PIC  9(01).
           03  TNSWK-SANSOKBN            PIC  9(01).
           03  TNSWK-TOKUKIZAISBT1       PIC  9(01).
           03  TNSWK-TOKUKIZAISBT2       PIC  9(01).
           03  TNSWK-JGNTEN              PIC  9(07).
           03  TNSWK-GAITENTTLKBN        PIC  9(03).
           03  TNSWK-NYUTENTTLKBN        PIC  9(03).
           03  TNSWK-CDKBN-KBN           PIC  X(01).
           03  TNSWK-CDKBN-SYO           PIC  9(01).
           03  TNSWK-CDKBN-BU            PIC  9(02).
           03  TNSWK-CDKBN-KBNNUM        PIC  9(03).
           03  TNSWK-CDKBN-KBNNUM-EDA    PIC  9(02).
      *R06.4 CHG
      *    03  TNSWK-CDKBN-KOUBAN        PIC  9(02).
           03  TNSWK-CDKBN-KOUBAN        PIC  9(03).
           03  TNSWK-KOKUCHI-KBN         PIC  X(01).
           03  TNSWK-KOKUCHI-SYO         PIC  9(01).
           03  TNSWK-KOKUCHI-BU          PIC  9(02).
      *(120)
           03  TNSWK-KOKUCHI-KBNNUM      PIC  9(03).
           03  TNSWK-KOKUCHI-KBNNUM-EDA  PIC  9(02).
           03  TNSWK-KOKUCHI-KOUBAN      PIC  9(02).
           03  TNSWK-KOKUJISKBKBN1       PIC  9(01).
           03  TNSWK-KOKUJISKBKBN2       PIC  9(01).
           03  TNSWK-NAMECHGKBN          PIC  9(01).
           03  TNSWK-KANANAMECHGKBN      PIC  9(01).
           03  TNSWK-IDOKANREN           PIC  X(09).
           03  TNSWK-KOUHYOJYUNNUM       PIC  9(09).
           03  TNSWK-YAKKAKJNCD          PIC  X(12).
           03  TNSWK-SYUSAISKB           PIC  9(01).
           03  TNSWK-SYOMEIKANREN        PIC  X(09).
      *R06.4 ADD START
      *    ����������ǯ����
           03  TNSWK-YAKKAKJNYMD         PIC  X(08).
      *    ����̾������
           03  TNSWK-GECODE              PIC  X(12).
      *    ����̾�����û��оݶ�ʬ
           03  TNSWK-IPNSHOHOKSN         PIC  X(01).
      *    ���ȣɣ�����ʬ
           03  TNSWK-HIVKBN              PIC  X(01).
      *R06.4 ADD END
           03  TNSWK-CHGYMD              PIC  X(08).
           03  TNSWK-HAISIYMD            PIC  X(08).
           03  TNSWK-KEIKASOCHIYMD       PIC  X(08).
           03  TNSWK-TERMID              PIC  X(16).
           03  TNSWK-OPID                PIC  X(16).
           03  TNSWK-CREYMD              PIC  X(08).
           03  TNSWK-UPYMD               PIC  X(08).
           03  TNSWK-UPHMS               PIC  X(06).
      *�����祭�������ѹ���
           03  TNSWK-UP-KEY.
               05  TNSWK-UP-HOSPNUM       PIC  9(02).
               05  TNSWK-UP-SRYCD         PIC  X(09).
               05  TNSWK-UP-YUKOSTYMD     PIC  X(08).
               05  TNSWK-UP-YUKOEDYMD     PIC  X(08).
      *
           03  TNSWK-STSRYKBN          PIC X(02).
           03  TNSWK-EDSRYKBN          PIC X(02).
           03  TNSXX-TK-CDKBN-KBNNUM   PIC  9(03).
      *    03  TNSXX-TK-CDKBN-KOUBAN   PIC  9(02).
           03  TNSXX-TK-CDKBN-KOUBAN   PIC  9(03).
           03  TNSXX-TK-TEN            PIC 9(09).
           03  TNSXX-TK-EDTEN          PIC 9(09).
