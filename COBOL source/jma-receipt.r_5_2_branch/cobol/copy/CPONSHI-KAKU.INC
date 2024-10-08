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
      *    ����饤���ʳ�ǧ�Ȳ��̥ơ��֥�
      *                                COPY  (CPONSHI-KAKU.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 05.01.00     ORCAMO       22/01/28  ����ե饰�ɲ�
      * 05.01.01     ORCAMO       22/12/09  �����ȯ�Է��֡���ѹ���
      *                                     �����ɲ�
      *                                     �����Ǿ������ͭ������
      *                                     ����ѹ�
      * 05.02.01    ORCAMO        23/12/21  �����޽��б�
      * 05.02.02    ORCAMO        24/04/08  �����޽�ʻ���б�
      *                                     ʻ�ѥե饰���������޽��գɣ��ɲ�
      ************************************************************
      *01  ONSKAKU-REC.
      *����
           03  ONSKAKU-KEY.
      *���ŵ��أɣ�
               05  ONSKAKU-HOSPNUM             PIC 9(02).
      *�գɣ�
               05  ONSKAKU-TBL-UUID            PIC X(36).
      *���գɣ�
           03  ONSKAKU-AITE-UUID           PIC X(36).
      *�ƣգɣ�
           03  ONSKAKU-OYA-UUID            PIC X(36).
      *����գɣ�
           03  ONSKAKU-KOUHI-UUID          PIC X(36).
      *�����޽��գɣ�
           03  ONSKAKU-FUJYO-UUID          PIC X(36).
      *�������޽��գɣ�
           03  ONSKAKU-FUJYO-UUID1         PIC X(36).
      *ʻ�ѥե饰
           03  ONSKAKU-FUJYO-HEIYO         PIC X(01).
      *���쥻�ѥ��顼������
           03  ONSKAKU-ORCA-ERR-CODE       PIC X(02).
      *PTID�����ʬ
           03  ONSKAKU-SET-PTID            PIC X(01).
      *���쥻���ͭ����
           03  ONSKAKU-ORCA-SIKAKU-YUKO    PIC X(01).
      *�����ե�����̾
           03  ONSKAKU-HKNOCR-FILENAME     PIC X(100).
      *��ʳ�ǧ��ȥ饤���
           03  ONSKAKU-KAKU-RENUMBER       PIC 9(02).
      *��ʳ�ǧ����ѥե饰
           03  ONSKAKU-KAKU-ENDFLG         PIC X(02).
      *��ʳ�ǧ����ǯ����
           03  ONSKAKU-KAKU-KAKUNINYMD     PIC X(08).
      *��ʳ�ǧ�������
           03  ONSKAKU-KAKU-KAKUNINHMS     PIC X(06).
      *�Ȳ��ֹ���Ͽ��ȥ饤���
           03  ONSKAKU-SHOU-RENUMBER       PIC 9(02).
      *�Ȳ��ֹ���Ͽ����ѥե饰
           03  ONSKAKU-SHOU-ENDFLG         PIC X(02).
      *���޾��������ȥ饤���
           03  ONSKAKU-YAKUZAI-RENUMBER    PIC 9(02).
      *���޾����������ѥե饰
           03  ONSKAKU-YAKUZAI-ENDFLG      PIC X(02).
      *���긡�Ǿ��������ȥ饤���
           03  ONSKAKU-KENSIN-RENUMBER     PIC 9(02).
      *���긡�Ǿ����������ѥե饰
           03  ONSKAKU-KENSIN-ENDFLG       PIC X(02).
      *
      *���ԣɣ�
           03  ONSKAKU-PTID                PIC  9(10).
      *ʣ������
           03  ONSKAKU-FUKUSU-GAITO        PIC  X(01).
      *�����¹�����
           03  ONSKAKU-SHORI-TIME          PIC  X(14).
      *��ʳ�ǧ��
           03  ONSKAKU-KAKUNINYMD          PIC  X(08).
      *���ŵ��إ�����
           03  ONSKAKU-HOSPCD              PIC  X(10).
      *Ǥ�դΥե����뼱�̻�(���ŵ��ظ�ͭ����)
           03  ONSKAKU-FILE-SIKIBETU       PIC  X(50).
      *�Ȳ��ʬ
           03  ONSKAKU-SHOKAI-KBN          PIC  X(01).
      *���쥻�Ȳ��ʬ
           03  ONSKAKU-ORCA-SHOKAI-KBN     PIC  X(01).
      *���Ŷ�ʬ
           03  ONSKAKU-ONSHINRYO-KBN       PIC  X(01).
      *������̶�ʬ
           03  ONSKAKU-RESULT-KBN          PIC  X(01).
      *���顼������
           03  ONSKAKU-ERR-CODE            PIC  X(09).
      *���顼��å�����
           03  ONSKAKU-ERR-MSG             PIC  X(120).
      *ʸ�������ɼ���
           03  ONSKAKU-CODE-TYPE           PIC  X(01).
      *�Ȳ�����
           03  ONSKAKU-SHOKAI-AREA.
      *�ݸ����ֹ�
               05  ONSKAKU-SHO-HKNJANUM         PIC  X(08).
      *����
               05  ONSKAKU-SHO-KIGO             PIC  X(80).
      *�ֹ�
               05  ONSKAKU-SHO-NUM              PIC  X(80).
      *�ֹ����
               05  ONSKAKU-SHO-EDABAN           PIC  X(02).
      *��ǯ����
               05  ONSKAKU-SHO-BIRTHDAY         PIC  X(08).
      *���ٳ�Ŭ��ǧ�����Ʊ�եե饰
               05  ONSKAKU-SHO-DOUIFLG          PIC  X(01).
      *�����ȯ�Է���(2022/7)
      *�����ȯ�Է���
           03  ONSKAKU-SHO-SHOHO-KEITAI         PIC  X(01).
      *������̾���
           03  ONSKAKU-RESULT              PIC  X(01).
      *������̥�����
           03  ONSKAKU-RESULT-CODE         PIC  X(09).
      *������̥�å�����
           03  ONSKAKU-RESULT-MSG          PIC  X(120).
      *
      *���ͭ����
           03  ONSKAKU-SIKAKU-YUKO         PIC  X(01).
      *
      *��ǧ���
           03  ONSKAKU-RESULT-AREA.
      *���ݸ��Ծڶ�ʬ(��ǧ�������)
               05  ONSKAKU-CARD-CLASS          PIC  X(02).
      *�ݸ����ֹ�
               05  ONSKAKU-RES-HKNJANUM        PIC  X(08).
      *����
               05  ONSKAKU-RES-KIGO            PIC  X(80).
      *�ֹ�
               05  ONSKAKU-RES-NUM             PIC  X(80).
      *�ֹ����
               05  ONSKAKU-RES-EDABAN          PIC  X(02).
      *�ܿͲ�²��ʬ
               05  ONSKAKU-RES-HONKZKKBN       PIC  X(01).
      *���ݸ��Ի�̾(���Ӽ��̾)
               05  ONSKAKU-RES-HIHKNJANAME     PIC  X(100).
      *��̾
               05  ONSKAKU-RES-NAME            PIC  X(100).
      *��̾�ʤ���¾��
               05  ONSKAKU-RES-NAME-ETC        PIC  X(100).
      *��̾����
               05  ONSKAKU-RES-KANANAME        PIC  X(100).
      *��̾���ʡʤ���¾��
               05  ONSKAKU-RES-KANANAME-ETC    PIC  X(100).
      *���̣�
               05  ONSKAKU-RES-SEX1            PIC  X(01).
      *���̣�
               05  ONSKAKU-RES-SEX2            PIC  X(01).
      *��ǯ����
               05  ONSKAKU-RES-BIRTHDAY        PIC  X(08).
      *����
               05  ONSKAKU-RES-ADRS            PIC  X(250).
      *͹���ֹ�
               05  ONSKAKU-RES-POST            PIC  X(08).
      *
      *���ݸ��Ծڸ���ǯ����
               05  ONSKAKU-SKKGETYMD           PIC  X(08).
      *���ݸ��Ծ�ͭ������ǯ����
               05  ONSKAKU-TEKSTYMD            PIC  X(08).
      *���ݸ��Ծ�ͭ����λǯ����
               05  ONSKAKU-TEKEDYMD            PIC  X(08).
      *���ݸ��Ծڰ�����ô����
               05  ONSKAKU-FTNRATE             PIC  X(03).
      *̤���ض�ʬ
               05  ONSKAKU-MISYUGAKU           PIC  X(01).
      *����Ӽ���ͳ
               05  ONSKAKU-LOSS-JIYU           PIC  X(02).
      *�ݸ���̾��
               05  ONSKAKU-HKNJANUM-NAME       PIC  X(128).
      *
      *�������Ծڸ���ǯ����
               05  ONSKAKU-ELDER-SKKGETYMD     PIC  X(08).
      *�������Ծ�ͭ������ǯ����
               05  ONSKAKU-ELDER-TEKSTYMD      PIC  X(08).
      *�������Ծ�ͭ����λǯ����
               05  ONSKAKU-ELDER-TEKEDYMD      PIC  X(08).
      *�������԰�����ô����
               05  ONSKAKU-ELDER-FTNRATE       PIC  X(03).
      *
      *�������ʣ������������������
               05  ONSKAKU-KOH-OCC             OCCURS  2.
      *����ͭ������ǯ����
                   07  ONSKAKU-KOH-TEKSTYMD    PIC  X(08).
      *����ͭ����λǯ����
                   07  ONSKAKU-KOH-TEKEDYMD    PIC  X(08).
      *������ô���
                   07  ONSKAKU-KOH-FTNRATE     PIC  9(03).
      *�����³�
                   07  ONSKAKU-KOH-JGNGAK      PIC  9(06).
      *
      *���ٳ�Ŭ��ǧ�����Ʊ�եե饰
               05  ONSKAKU-GENDO-DOUIFLG       PIC  X(01).
      *���ٳ�Ŭ��ǧ�����Ʊ������
               05  ONSKAKU-GENDO-TIME          PIC  X(14).
      *���ٳ�Ŭ��ǧ��ڶ�ʬ
               05  ONSKAKU-GENDO-SHOKBN        PIC  X(02).
      *���ٳ�Ŭ��ǧ���Ŭ�Ѷ�ʬ
               05  ONSKAKU-GENDO-TEKKBN        PIC  X(03).
      *���ٳ�Ŭ��ǧ��ڸ���ǯ����
               05  ONSKAKU-GENDO-SKKGETYMD     PIC  X(08).
      *���ٳ�Ŭ��ǧ���ͭ������ǯ����
               05  ONSKAKU-GENDO-TEKSTYMD      PIC  X(08).
      *���ٳ�Ŭ��ǧ���ͭ����λǯ����
               05  ONSKAKU-GENDO-TEKEDYMD      PIC  X(08).
      *���ٳ�Ŭ��ǧ���Ĺ����������ǯ����
               05  ONSKAKU-GENDO-SKJGNSTYMD    PIC  X(08).
      *
      *����������ܼ��ž���Ʊ�եե饰
               05  ONSKAKU-SIKKAN-DOUIFLG      PIC  X(01).
      *����������ܼ��ž���Ʊ������
               05  ONSKAKU-SIKKAN-TIME         PIC  X(14).
      *����������ܼ��ž�ǧ�����
               05  ONSKAKU-SIKKAN-AREA.
                   07  ONSKAKU-SIKKAN-OCC     OCCURS   3.
      *����������ܼ��ž�ǧ����¶�ʬ
                       09  ONSKAKU-SIKKAN-SIPKBN       PIC  X(01).
      *����������ܼ��žڸ���ǯ����
                       09  ONSKAKU-SIKKAN-SKJGNSTYMD   PIC  X(08).
      *����������ܼ��ž�ͭ������ǯ����
                       09  ONSKAKU-SIKKAN-TEKSTYMD     PIC  X(08).
      *����������ܼ��ž�ͭ����λǯ����
                       09  ONSKAKU-SIKKAN-TEKEDYMD     PIC  X(08).
      *����������ܼ��žڼ�����ô���ٳ�
                       09  ONSKAKU-SIKKAN-FTNMONEY     PIC  X(06).
      *�����Ǿ���
               05  ONSKAKU-KENSIN-AREA.
      *�����Ǿ������Ʊ�եե饰
                   07  ONSKAKU-KENSIN-DOUIFLG      PIC  X(01).
      *�����Ǿ������Ʊ������
                   07  ONSKAKU-KENSIN-TIME         PIC  X(14).
      *�����Ǿ������ͭ������
                   07  ONSKAKU-KENSIN-KIGENYMD     PIC  X(14).
      *���޾������
               05  ONSKAKU-YAKUZAI-AREA.
      *���޾������Ʊ�եե饰
                   07  ONSKAKU-YAKUZAI-DOUIFLG     PIC  X(01).
      *���޾������Ʊ������
                   07  ONSKAKU-YAKUZAI-TIME        PIC  X(14).
      *���޾������ͭ������
                   07  ONSKAKU-YAKUZAI-KIGEN       PIC  X(14).
      *
      *���ž������
               05  ONSKAKU-SHINRYO-AREA.
      *���ž������Ʊ�եե饰
                   07  ONSKAKU-SHINRYO-DOUIFLG     PIC  X(01).
      *���ž������Ʊ������
                   07  ONSKAKU-SHINRYO-TIME        PIC  X(14).
      *���ž������ͭ������
                   07  ONSKAKU-SHINRYO-KIGEN       PIC  X(14).
      *
      *��Ѿ������
               05  ONSKAKU-OPE-AREA.
      *���ž������Ʊ�եե饰
                   07  ONSKAKU-OPE-DOUIFLG         PIC  X(01).
      *��Ѿ������Ʊ������
                   07  ONSKAKU-OPE-TIME            PIC  X(14).
      *��Ѿ������ͭ������
                   07  ONSKAKU-OPE-KIGEN           PIC  X(14).
      *
      *Ǥ�դμ��̻�(���ŵ��ظ�ͭ����)
           03  ONSKAKU-SIKIBETU            PIC  X(50).
      *�Ȳ��ֹ�
           03  ONSKAKU-SHOKAI-NUM          PIC  X(50).
      *�����Ѥߥե饰
           03  ONSKAKU-MOD-FLG             PIC  X(01).
      *�������ʬ
           03  ONSKAKU-LOCK-KBN            PIC  X(01).
      *����ե饰
           03  ONSKAKU-SCR-DLTFLG          PIC  X(01).
      *ü���ɣ�
           03  ONSKAKU-TERMID              PIC  X(64).
      *���ڡ��졼���ɣ�
           03  ONSKAKU-OPID                PIC  X(16).
      *����ǯ����
           03  ONSKAKU-CREYMD              PIC  X(08).
      *��������
           03  ONSKAKU-CREHMS              PIC  X(06).
      *����ǯ����
           03  ONSKAKU-UPYMD               PIC  X(08).
      *��������
           03  ONSKAKU-UPHMS               PIC  X(06).
      *LIMIT
           03  ONSKAKU-SELLIMIT            PIC  9(08).
      *OFFSET
           03  ONSKAKU-SELOFFSET           PIC  9(08).
