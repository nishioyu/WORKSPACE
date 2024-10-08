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
      *         �����ޥ���    COPY (CPTENSU.INC)        *
      *                       RECORD LEN  650 BYTE      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     MCC-¿��Ǽ   02/03/15  ��ȯ�����ʶ�ʬ��
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
      * 05.00.00     NACL-��ƣ    17/01/05  �ޥ�������ʬ
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
       01  TNS-TENSU-REC.
      *(1)
           03  TNS-KEY.
               05  TNS-HOSPNUM         PIC 9(2).
               05  TNS-SRYCD           PIC  X(09).
               05  TNS-YUKOSTYMD       PIC  X(08).
               05  TNS-YUKOEDYMD       PIC  X(08).
           03  TNS-SRYKBN              PIC  X(02).
           03  TNS-SRYSYUKBN           PIC  X(03).
           03  TNS-YUKOKETA            PIC  9(02).
           03  TNS-NAME                PIC  X(200).
           03  TNS-KANAYUKOKETA        PIC  9(02).
           03  TNS-KANANAME            PIC  X(200).
           03  TNS-FORMALYUKOKETA      PIC  9(03).
           03  TNS-FORMALNAME          PIC  X(400).
           03  TNS-TENSIKIBETU         PIC  9(01).
           03  TNS-TEN                 PIC  9(09)V99.
           03  TNS-TANICD              PIC  X(03).
           03  TNS-TANIMOJISU          PIC  9(01).
           03  TNS-TANINAME            PIC  X(24).
           03  TNS-DATAKBN             PIC  9(01).
           03  TNS-HKNTEKKBN           PIC  9(01).
           03  TNS-NYUGAITEKKBN        PIC  9(01).
      *(20)
           03  TNS-ROUTEKKBN           PIC  9(01).
           03  TNS-HOSPSRYKBN          PIC  9(01).
           03  TNS-OSINKBN             PIC  9(01).
           03  TNS-HOUKNSKBN           PIC  9(02).
           03  TNS-BYOKANRENKBN        PIC  9(02).
           03  TNS-SDOKNRYO            PIC  9(02).
           03  TNS-JITUDAY             PIC  9(01).
           03  TNS-DAYCNT              PIC  9(01).
           03  TNS-YKZKNRKBN           PIC  9(01).
           03  TNS-KZMCOMPSIKIBETU     PIC  9(01).
           03  TNS-KZMKGN              PIC  9(08).
           03  TNS-KZMJGN              PIC  9(08).
           03  TNS-KZM                 PIC  9(08).
           03  TNS-KZMTEN              PIC  9(07)V99.
           03  TNS-KZMERR              PIC  9(01).
           03  TNS-JGNCNT              PIC  9(03).
           03  TNS-JGNCNT1D            PIC  9(03).
           03  TNS-JGNCNT1W            PIC  9(03).
           03  TNS-JGNCNTETCM          PIC  9(02).
           03  TNS-JGNCNTETC           PIC  9(03).
      *(40)
           03  TNS-JGNCNTERR           PIC  9(01).
           03  TNS-CHUKSNCD            PIC  X(04).
           03  TNS-CHUKSNTUBAN         PIC  X(01).
           03  TNS-TSUSOKUAGE          PIC  9(01).
           03  TNS-KGNAGE              PIC  X(02).
           03  TNS-JGNAGE              PIC  X(02).
           03  TNS-TIMEKSNKBN          PIC  9(01).
      *    �����Ŭ��������ʬ
           03  TNS-KIJUNTEIGENKBN      PIC  9(01).
      *    �����Ŭ�������оݻ��ߴ�ॳ����
           03  TNS-KIJUNTEIGENCD       PIC  9(04).
           03  TNS-LASERKBN            PIC  9(01).
           03  TNS-CHPMESUKSN          PIC  9(01).
           03  TNS-MICMESUKSN          PIC  9(01).
           03  TNS-DONORKBN            PIC  9(01).
           03  TNS-KNSJISKBN           PIC  9(01).
           03  TNS-KNSJISGRPKBN        PIC  9(02).
           03  TNS-TEIGENKBN           PIC  9(01).
           03  TNS-SEKIZUISOKUTEI      PIC  9(01).
           03  TNS-KEIBUJYUTU          PIC  9(01).
           03  TNS-AUTOHOUGO           PIC  9(01).
           03  TNS-GAIKANRIKBN         PIC  9(01).
           03  TNS-TUSOKUGAIKBN        PIC  9(01).
           03  TNS-HOUKATUTEIGENKBN    PIC  9(03).
           03  TNS-CHPNAISIKSN         PIC  9(01).
      *(60)
           03  TNS-AUTOFUNGO           PIC  9(01).
           03  TNS-CHIIKIKSN           PIC  9(01).
           03  TNS-BYOSYOKBN           PIC  9(01).
           03  TNS-CHPGYOKOSOTIKSN     PIC  9(01).
           03  TNS-SHORTSTAYOPE        PIC  9(01).
           03  TNS-BUIKBN              PIC  9(01).
           03  TNS-SANTEIRRKKBN        PIC  9(01).
           03  TNS-SSTKIJUNCDTBL       OCCURS 10.
               05  TNS-SSTKIJUNCD      PIC  9(04).
           03  TNS-AGEKSNTBL           OCCURS 4.
               05  TNS-AGEKSNKGN       PIC  X(02).
               05  TNS-AGEKSNJGN       PIC  X(02).
               05  TNS-AGEKSNSRYCD     PIC  X(09).
           03  TNS-KENTAICOMMENT       PIC  9(01).
      *(90)
           03  TNS-NYUKHNKBN           PIC  9(02).
           03  TNS-NYUKHNKSNKBN        PIC  9(03).
           03  TNS-KANGOKSN            PIC  9(02).
           03  TNS-GAZOOPESUP          PIC  9(01).
           03  TNS-IRYOKANSATUKBN      PIC  9(01).
           03  TNS-MASUISKBKBN         PIC  9(01).
      *H26.4 ADD START
           03  TNS-FUKUBIKUNAIKBN      PIC  9(01).
           03  TNS-FUKUBIKUKOTUKBN     PIC  9(01).
           03  TNS-TIMEMASUIKBN        PIC  9(01).
           03  TNS-DPCKBN              PIC  9(01).
      *H26.4 ADD END
      *H28.4 ADD START
      *    �󿯽�Ū���ư�֥�˥���󥰲û�
           03  TNS-HISINSYUMONITORKSN  PIC  9(01).
      *    �����¸Ʊ���ȿ��û�
           03  TNS-TOUKETUHOZONKSN     PIC  9(01).
      *    ����ɽ��ʬ�ֹ�
           03  TNS-KUBUNBANGOU         PIC  X(30).
      *    ϫ�һ����Բ�
           03  TNS-ROSAIKBN            PIC  9(01).
      *    �ͻ�û���ϫ�ҡ�
           03  TNS-SISIKSN             PIC  9(01).
      *H28.4 ADD END
      *H30.4 ADD START
      *    �������������ȿ�ɸ�ܲû�
           03  TNS-AKUSEIBYORIKSN      PIC  9(01).
      *H30.4 ADD END
      *R02.4 ADD START
      *    �ϳ������û�
           03  TNS-SOGAIKOTEIKSN       PIC  9(01).
      *    Ķ�����ں��û�
           03  TNS-CHPSESSAKUKSN       PIC  9(01).
      *    �������ĺ���ʻ�ܶ�ʬ
           03  TNS-SASINJIHEISIKBN     PIC  9(01).
      *R02.4 ADD END
      *R04.4 ADD START
      *    ���贶�����к�����û���
      *R06.4 CHG
      *    03  TNS-GAIKANSENSYOKSN     PIC  9(01).
           03  TNS-GAIKANSENSYOKSN     PIC  X(01).
      *    ��ɡ���������Ļ����ֲû�
           03  TNS-JIBINYUYOJIKSN      PIC  9(01).
      *    ��ɡ�����ʾ���������Ŭ�����ѻٱ�û�
           03  TNS-JIBISYONIKOKINKSN   PIC  9(01).
      *    �ڳ��϶ɽ걢���ĺ���������û�
           03  TNS-SEKKAISOUKIKIKSN    PIC  9(01).
      *R04.4 ADD END
      *R06.4 ADD START
      *    �Ǹ���������ɾ������
           03  TNS-KANGOHYOKA          PIC  X(01).
      *    ���衦����١������å�ɾ�����ʣ���
           03  TNS-BASEUPHYOKA1        PIC  X(01).
      *    ���衦����١������å�ɾ�����ʣ���
           03  TNS-BASEUPHYOKA2        PIC  X(01).
      *    ����¤ñ����Ѱ��ŵ�����Ѳû�
           03  TNS-RSUDKSN             PIC  9(03).
      *R06.4 ADD END
           03  TNS-OLDTENSKB           PIC  9(01).
           03  TNS-OLDTEN              PIC  9(09)V99.
           03  TNS-MADOKUKBN           PIC  9(01).
           03  TNS-SINKEIHAKAIKBN      PIC  9(01).
           03  TNS-SEIBUTUGAKUKBN      PIC  9(01).
           03  TNS-ZOEIZAIKBN          PIC  9(01).
           03  TNS-CSYYOURYO           PIC  9(05).
      *(100)
           03  TNS-YKZKBN              PIC  9(01).
           03  TNS-ZAIGATAKBN          PIC  9(01).
      *    ��ȯ�����ʶ�ʬ
           03  TNS-KOUHATUKBN          PIC  9(01).
           03  TNS-LONGTOYOKBN         PIC  9(01).
           03  TNS-MEIUSESKB           PIC  9(01).
           03  TNS-TOKUKIZAIAGEKSNKBN  PIC  9(01).
           03  TNS-SANSOKBN            PIC  9(01).
           03  TNS-TOKUKIZAISBT1       PIC  9(01).
           03  TNS-TOKUKIZAISBT2       PIC  9(01).
           03  TNS-JGNTEN              PIC  9(07).
           03  TNS-GAITENTTLKBN        PIC  9(03).
           03  TNS-NYUTENTTLKBN        PIC  9(03).
           03  TNS-CDKBN-KBN           PIC  X(01).
           03  TNS-CDKBN-SYO           PIC  9(01).
           03  TNS-CDKBN-BU            PIC  9(02).
           03  TNS-CDKBN-KBNNUM        PIC  9(03).
           03  TNS-CDKBN-KBNNUM-EDA    PIC  9(02).
      *R06.4 CHG
      *    03  TNS-CDKBN-KOUBAN        PIC  9(02).
           03  TNS-CDKBN-KOUBAN        PIC  9(03).
           03  TNS-KOKUCHI-KBN         PIC  X(01).
           03  TNS-KOKUCHI-SYO         PIC  9(01).
           03  TNS-KOKUCHI-BU          PIC  9(02).
      *(120)
           03  TNS-KOKUCHI-KBNNUM      PIC  9(03).
           03  TNS-KOKUCHI-KBNNUM-EDA  PIC  9(02).
           03  TNS-KOKUCHI-KOUBAN      PIC  9(02).
           03  TNS-KOKUJISKBKBN1       PIC  9(01).
           03  TNS-KOKUJISKBKBN2       PIC  9(01).
      *(09593XXXX �����ս������谷�ʳ���ˤǻ���)
           03  TNS-NAMECHGKBN          PIC  9(01).
      *(09593XXXX �����ս������谷�������ˤǻ���)
           03  TNS-KANANAMECHGKBN      PIC  9(01).
           03  TNS-IDOKANREN           PIC  X(09).
           03  TNS-KOUHYOJYUNNUM       PIC  9(09).
           03  TNS-YAKKAKJNCD          PIC  X(12).
           03  TNS-SYUSAISKB           PIC  9(01).
           03  TNS-SYOMEIKANREN        PIC  X(09).
      *R06.4 ADD START
      *    ����������ǯ����
           03  TNS-YAKKAKJNYMD         PIC  X(08).
      *    ����̾������
           03  TNS-GECODE              PIC  X(12).
      *    ����̾�����û��оݶ�ʬ
           03  TNS-IPNSHOHOKSN         PIC  X(01).
      *    ���ȣɣ�����ʬ
           03  TNS-HIVKBN              PIC  X(01).
      *R06.4 ADD END
           03  TNS-CHGYMD              PIC  X(08).
           03  TNS-HAISIYMD            PIC  X(08).
           03  TNS-KEIKASOCHIYMD       PIC  X(08).
           03  TNS-TERMID              PIC  X(16).
           03  TNS-OPID                PIC  X(16).
           03  TNS-CREYMD              PIC  X(08).
           03  TNS-UPYMD               PIC  X(08).
           03  TNS-UPHMS               PIC  X(06).
           03  TNS-MASTER-CLASS        PIC  9(01).
      *�����祭�������ѹ���
           03  TNSUP-KEY.
               05  TNSUP-HOSPNUM       PIC 9(2).
               05  TNSUP-SRYCD         PIC  X(09).
               05  TNSUP-YUKOSTYMD     PIC  X(08).
               05  TNSUP-YUKOEDYMD     PIC  X(08).
      *
           03  TNSXX-STSRYKBN          PIC X(02).
           03  TNSXX-EDSRYKBN          PIC X(02).
           03  TNSXX-TK-CDKBN-KBNNUM   PIC  9(03).
      *    03  TNSXX-TK-CDKBN-KOUBAN   PIC  9(02).
           03  TNSXX-TK-CDKBN-KOUBAN   PIC  9(03).
           03  TNSXX-TK-TEN            PIC 9(09).
           03  TNSXX-TK-EDTEN          PIC 9(09).
