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
      *    ���Уɥ쥹�ݥ󥹴����ݸ��ȹ礻����(xml2)
      *                                   COPY (CPPATIENTLST6V2RES.INC)
      ******************************************************************
        01  XML-PATIENTLST6RES.
            02  PTRES-CONTEXT          PIC S9(9)   BINARY.
            02  PTRES-OBJECT           PIC X(36).
            02  PTRES-MODE             PIC S9(9)   BINARY.
            02  PTRES-RECORDNAME       PIC X(128).
            02  PTRES-PATIENTINFORES.
      *       �»���
              03  PTRES-INFORMATION-DATE   PIC X(10).
      *       �»ܻ���
              03  PTRES-INFORMATION-TIME   PIC X(8).
      *       �ֵѥ�����
              03  PTRES-API-RESULT         PIC X(3).
      *       �ֵѥ�å�����
              03  PTRES-API-RESULT-MSG     PIC X(100).
              03  PTRES-RESKEY             PIC X(100).
      *       ���Ծ���
              03  PTRES-PATIENT-INFOMATION.
      *           �����ֹ�
                  04  PTRES-PATIENTID          PIC X(20).
      *           ������̾
                  04  PTRES-NAME               PIC X(100).
      *           ���ʻ�̾
                  04  PTRES-KANANAME           PIC X(100).
      *           ��ǯ����
                  04  PTRES-BIRTHDAY           PIC X(10).
      *           ����
                  04  PTRES-SEX                PIC X(1).
      *       �ݸ��ȹ礻����
              03  PTRES-INSURANCE-INFOMATION.
      *           �ݸ��ȹ礻����
                  04  PTRES-INSURANCE-INFO     OCCURS  200 TIMES.
      *               �ݸ��ȹ礻�ֹ�
                      06  PTRES-COMB-NUMBER            PIC X(04).
      *               �ݸ��ȹ礻����ô����������
                      06  PTRES-COMB-RATE-ADM          PIC X(04).
      *               �ݸ��ȹ礻����ô���ʳ����
                      06  PTRES-COMB-RATE-OUT          PIC X(04).
      *               �ݸ��ȹ礻������
                      06  PTRES-COMB-START-DATE        PIC X(10).
                      06  PTRES-COMB-END-DATE          PIC X(10).
      *               ��ɽ����ʬ
                      06  PTRES-COMBINATION-HYOJI      PIC X(01).
      *
      *               �ݸ��μ���
                      06  PTRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               �ݸ����ֹ�
                      06  PTRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *               �ݸ��μ���̾��
                      06  PTRES-MAIN-INSURANCE-NAME    PIC X(20).
      *               ����
                      06  PTRES-MAIN-MARK              PIC X(80).
      *               �ֹ�
                      06  PTRES-MAIN-NUMBER            PIC X(80).
      *R02.12         ����
                      06  PTRES-MAIN-EDABAN            PIC X(02).
      *               ��³��ʬ
                      06  PTRES-MAIN-CONTINUATION      PIC X(01).
      *               �����ʬ
                      06  PTRES-MAIN-ASSISTANCE        PIC X(01).
      *               �����ʬ̾��
                      06  PTRES-MAIN-ASSISTANCE-NAME   PIC X(20).
      *               �ܿͲ�²��ʬ
                      06  PTRES-MAIN-FAMILY            PIC X(01).
      *               ���ݸ���̾
                      06  PTRES-MAIN-POLICYHOLDER      PIC X(100).
      *               ͭ��������
                      06  PTRES-MAIN-START-DATE        PIC X(10).
      *               ͭ����λ��
                      06  PTRES-MAIN-END-DATE          PIC X(10).
      *               ��ʼ�����
                      06  PTRES-MAIN-GET-DATE          PIC X(10).
      *               �ǽ���ǧ��
                      06  PTRES-MAIN-CHECK-DATE        PIC X(10).
      *
      *               �������
                      06  PTRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *                 ����μ���
                        07  PTRES-INSURANCE-CLASS         PIC X(03).
      *                 ����μ���̾��
                        07  PTRES-INSURANCE-NAME          PIC X(20).
      *                 ��ô���ֹ�
                        07  PTRES-PROVIDER-NUMBER         PIC X(08).
      *                 ������ֹ�
                        07  PTRES-RECIPIENT-NUMBER        PIC X(20).
      *                 ��������ôΨ�ʳ��
                        07  PTRES-RATE-ADMISSION          PIC X(04).
      *                 �����ݸ����
                        07  PTRES-MONEY-ADMISSION         PIC X(06).
      *                 �������ôΨ�ʳ��
                        07  PTRES-RATE-OUTPATIENT         PIC X(04).
      *                 ����ݸ����
                        07  PTRES-MONEY-OUTPATIENT        PIC X(06).
      *                 ͭ��������
                        07  PTRES-INSURANCE-START-DATE    PIC X(10).
      *                 ͭ����λ��
                        07  PTRES-INSURANCE-END-DATE      PIC X(10).
      *R01.06
      *                 �ǽ���ǧ��
                        07  PTRES-INSURANCE-CHECK-DATE    PIC X(10).
      *ϫ�Ҿ���
                      06  PTRES-ACCIDENT-INF-G.
      *                  ϫ���ݸ�̾�Ρʵ����Ʊ�����ơ�
                         07  PTRES-ACCIDENT-NAME           PIC X(50).
      *                  ���¤����̡��ֹ��Ʊ�����ơ�
                         07  PTRES-DISEASE-LOCATION        PIC X(100).
      *                  ����ǯ����
                         07  PTRES-DISEASE-DATE            PIC X(10).
      *ϫ��
      *                  ϫƯ�ݸ��ֹ�
                         07  PTRES-ACCIDENT-NUMBER         PIC X(14).
      *                  ǯ��ڽ��ֹ�
                         07  PTRES-PENSIONCERTIFICATE      PIC X(09).
      *                  �ҳ���ʬ
                         07  PTRES-ACCIDENT-CLASS          PIC X(01).
      *                  �ҳ���ʬ��̾�Ρ�
                         07  PTRES-ACCIDENT-CLASS-NAME     PIC X(20).
      *                  ϫƯ�����Ľ𥳡���
                         07  PTRES-LABOR-ST-CODE           PIC X(05).
      *                  ϫƯ�����Ľ𥳡���
                         07  PTRES-LABOR-ST-NAME           PIC X(50).
      *
      *                  ���Ѻ��̶�ʬ
                         07  PTRES-SINKEI                  PIC X(01).
                         07  PTRES-SINKEI-NAME             PIC X(10).
      *                  ž����ͳ
                         07  PTRES-TENKI                   PIC X(01).
                         07  PTRES-TENKI-NAME              PIC X(10).
      *                  �ͻ������ʬ
                         07  PTRES-SISIKBN                 PIC X(01).
                         07  PTRES-SISIKBN-NAME            PIC X(10).
      *
      *                  ���Ƚ�̾��
                         07  PTRES-RSI-JIGYOUNAME          PIC X(80).
      *                  �������ƻ�ܸ�̾
                         07  PTRES-RSI-PREFNAME            PIC X(20).
      *                  �������ƻ�ܸ���ʬ
                         07  PTRES-RSI-PREFKBN             PIC X(01).
      *                  �������ƻ�ܸ���ʬ̾��
                         07  PTRES-RSI-PREFKBNNAME         PIC X(02).
      *                  ����Ϸ��Զ�̾
                         07  PTRES-RSI-CITYNAME            PIC X(80).
      *                  ����Ϸ��Զ��ʬ
                         07  PTRES-RSI-CITYKBN             PIC X(01).
      *                  ����Ϸ��Զ�̾��
                         07  PTRES-RSI-CITYKBNNAME         PIC X(02).
      *                  ϫ�ҥ쥻������� ���ǯ��
                         07  PTRES-RSI-KIJYUNYM            PIC X(07).
      *                  ϫ�ҥ쥻������� ���
                         07  PTRES-RESEPUTO-COUNT          PIC X(03).
      *������
      *                  �����������ʬ
                         07  PTRES-JIBAISEIKBN             PIC X(01).
                         07  PTRES-JIBAISEIKBN-NAME        PIC X(20).
      *                  �������ݸ����̾
                         07  PTRES-RSI-HOKENNAME           PIC X(80).
      *                  ������ô����
                         07  PTRES-RSI-DRCD                PIC X(05).
      *                  ������ô����̾��
                         07  PTRES-RSI-DRCD-NAME           PIC X(100).
      *���ե�������
      *                  �򹯴�����Ģ�ֹ�
                         07  PTRES-RSI-KANRITCHO-NUMBER    PIC X(13).
      *                  »����ʬ
                         07  PTRES-DAMAGE-CLASS            PIC X(03).
      *                  »����ʬ̾��
                         07  PTRES-DAMAGE-CLASS-NAME       PIC X(100).
      *�軰�Թ԰�
      *                  ��ʪ�ٵ��ʬ
                         07  PTRES-DS-GENBTKBN             PIC X(01).
                         07  PTRES-DS-GENBTKBN-NAME        PIC X(20).
      *                  �õ������ʬ
                         07  PTRES-DS-TOKKIKBN             PIC X(01).
                         07  PTRES-DS-TOKKIKBN-NAME        PIC X(20).
      *
      *
