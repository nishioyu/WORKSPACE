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
      *    �����쥻�ץȡ����ž���         COPY (CPRCF130.INC) 
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.01    NACL-ƣ��    09/07/14  �������ʲ������б�
      *  04.07.01    NACL-����    14/09/11  �����ޥ����ο��Ŷ�ʬ�ɲ�
      *
      *  04.08.01    NACL-����    16/01/12  �����ѥ쥻�ץȱ�������
      *                                     ����̾�����б�
      *  04.08.02    NACL-����    16/04/20  ʿ������ǯ��������б�
      *
      *  05.00.01    NACL-����    20/03/30  ���£�ǯ��������б�
      *  05.00.02    NACL-����    20/05/19  ���£�ǯ��������б�
      *
      *  05.01.01    ORCAMO       22/06/08  ��쥻�ץȤε��ܽ��б�
      *                                     �ʿ��Ŷ�ʬ������������
      *  05.01.02    ORCAMO       22/07/11  �����������򼰥������б�
      *  05.02.01    ORCAMO       24/04/26  ���£�ǯ�ٲ����б�
      *                                     CDKBN-KOUBAN�򣲷夫�飳����ѹ�
      ************************************************************
      *01  RECE130-REC.
           03  RECE130-KEY.
               05  RECE130-KEY1.
      *���ŵ����ֹ�
                   07  RECE130-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE130-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE130-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE130-PTID        PIC 9(10).
      *�쥻�ץȿ��Ų�
                   07  RECE130-RECEKA      PIC X(02).
      *����ɣ�
                   07  RECE130-HKNID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE130-TEKSTYMD    PIC X(08). 
      *���ֹ�
               05  RECE130-ZAINUM          PIC 9(08).
      *Ϣ�ֹ�
               05  RECE130-RENNUM          PIC 9(02).
      *�����ֹ�
           03  RECE130-PTNUM               PIC X(20).
      *���Ų�
           03  RECE130-SRYKA               PIC X(02).
      *���Ŷ�ʬ
           03  RECE130-SRYKBN              PIC X(02).
      *���ż��̶�ʬ
           03  RECE130-SRYSYUKBN           PIC X(03).
      *������
           03  RECE130-JIHIMONEY           PIC 9(07).
      *
           03  RECE130-SRYACT-TBL          OCCURS  5.
      *���Ź԰پ���
               05  RECE130-SRYACTINF.
      *���ť�����
                   07  RECE130-SRYCD           PIC X(09).
      *���ż��̶�ʬ
                   07  RECE130-SRYSYUKBN-X     PIC X(03).
      *����
                   07  RECE130-SRYSURYO        PIC 9(05)V9(05).
      *���
                   07  RECE130-SRYKAISU        PIC 9(03).
      *ñ�̥�����
                   07  RECE130-TANICD          PIC 9(03).
      *ñ��̾��ͭ�����
                   07  RECE130-TANIMOJISU      PIC 9(01).
      *ñ��̾��
                   07  RECE130-TANINAME        PIC X(24).
      *�������̶�ʬ
                   07  RECE130-TENSIKIBETU     PIC 9(01).
      *����
                   07  RECE130-TEN             PIC 9(09)V9(02).
      *��������ե饰
                   07  RECE130-MEISKYFLG       PIC X(01).
      *��ư�����ʬ
                   07  RECE130-AUTOKBN         PIC X(01).
      *̾�������ֹ�
                   07  RECE130-INPUTNUM        PIC 9(03).
      *�����󽸷��輱��
                   07  RECE130-TENTTLKBN       PIC 9(03).
      *�ǡ�����ʬ
                   07  RECE130-DATAKBN         PIC 9(01).
      *����оݸ���
                   07  RECE130-HOUKNSKBN       PIC 9(02).
      *�������»�Ƚ��
                   07  RECE130-KNSJISKBN       PIC 9(01).
      *�������»�Ƚ�ǥ��롼��
                   07  RECE130-KNSJISGRPKBN    PIC 9(02).
      *�Ƽ��ֹ���
                   07  RECE130-CDKBNINF.
                       09  RECE130-CDKBN-KBN        PIC X(01).
                       09  RECE130-CDKBN-SYO        PIC 9(01).
                       09  RECE130-CDKBN-BU         PIC 9(02).
                       09  RECE130-CDKBN-KBNNUM     PIC 9(03).
                       09  RECE130-CDKBN-KBNNUM-EDA PIC 9(02).
                       09  RECE130-CDKBN-KOUBAN     PIC 9(03).
                       09  RECE130-KOUHYOJYUNNUM    PIC 9(09).
      *�������̶�ʬ��
                   07  RECE130-KOKUJISKBKBN1   PIC 9(01).
      *�������̶�ʬ��
                   07  RECE130-KOKUJISKBKBN2   PIC 9(01).
      *�������ͷ׻�����
                   07  RECE130-KZMCOMPSIKIBETU PIC 9(01).
      *���û�������
                   07  RECE130-CHUKSNINF.
      *������
                       09  RECE130-CHUKSNCD     PIC X(04).
      *����
                       09  RECE130-CHUKSNTUBAN  PIC X(01).
      *���θ���������
                   07  RECE130-KENTAICOMMENT    PIC 9(01).
      *���ϥ�����
                   07  RECE130-INPUTCOMENT      PIC X(140).
      *̾�ξ���
               05  RECE130-NAMEINF.
      *����ͭ�����
                   07  RECE130-YUKOKETA         PIC 9(03).
      *����̾��
                   07  RECE130-NAME             PIC X(300).
      *����¾����
               05  RECE130-SONOTAINF.
      *��������̣�
                   07  RECE130-TOKUKIZAISBT1    PIC 9(01).
      *��������ʬ
                   07  RECE130-SANSOKBN         PIC 9(01).
      *
      *�쥻�ץ��оݳ��ե饰
                   07  RECE130-RECEPTFLG        PIC X(01).
      *�쥻�������ྦ��̾�Խ��ե饰
                   07  RECE130-RECEHENFLG       PIC X(01).
      *�����ޥ����ο��Ŷ�ʬ
                   07  RECE130-TNSSRYKBN        PIC X(02).
      *�����ȥ������֤������ե饰
                   07  RECE130-COMCDCHGFLG      PIC X(01).
      *
      *����̾���ܱ��������ե饰
                   07  RECE130-INGAIFLG         PIC X(01).
      *����̾���ܱ��������ե饰�ʥ쥻�ץ��ѡ�
                   07  RECE130-INGAIFLG-RECE    PIC X(01).
      *����̾���ܱ��������ե饰�ʼ������������ѡ�
                   07  RECE130-INGAIFLG-SIPPU   PIC X(01).
      *����̾���ܱ��������ե饰���������򼰥������ѡ�
                   07  RECE130-INGAIFLG-TSC     PIC X(01).
      *����̾�����ɡ�������ॳ���ɡ�
                   07  RECE130-YAKKAKJNCD       PIC X(12).
      *����̾�����û���ʬ
                   07  RECE130-GENERIC-KBN      PIC X(01).
      *����̾��
                   07  RECE130-GENERICNAME      PIC X(200).
      *����ͭ�����
                   07  RECE130-GENERICYUKOKETA  PIC 9(03).
      *
      *����������
                   07  RECE130-SIPPUFLG         PIC X(01).
      *�����������򼰥����Ⱦ���
                   07  RECE130-TOYAKUSENCOMFLG  PIC X(01).
      *
