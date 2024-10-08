      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  19/01/XX  複数科保険対応
      *****************************************************************
      ******************************************************************
      *    ＡＰＩリクエスト診療行為チェック情報 (一体化)
      *                                       COPY (CPMDCXMLV3REQ2.INC)
      *         診療行為内容
      ******************************************************************
       01  XML-MEDICALV3REQ2.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       リクエスト番号
              05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *       UID（電子カルテ）
              05  MDCREQ-KARTE-UID         PIC X(36).
      *       基準日付（収納入金日）
              05  MDCREQ-BASE-DATE         PIC X(10).
      *       患者番号
              05  MDCREQ-PATIENTID         PIC X(20).
      *       診療日付
              05  MDCREQ-PERFORM-DATE      PIC X(10).
      *       診療時間
              05  MDCREQ-PERFORM-TIME      PIC X(8).
      *       UID（オルカ）
              05  MDCREQ-ORCA-UID          PIC X(36).
      *H27.12
      *       処理区分
              05  MDCREQ-PATIENT-MODE      PIC X(06).
      *
      *       印刷モード 2019/09/19 Added
              05  MDCREQ-PRINT-MODE        PIC X(10).
      *       伝票番号
              05  MDCREQ-INVOICD-NUMBER    PIC X(07).
      *
      *       確認処理区分（１：最初の確認メッセージから）
              05  MDCREQ-SELECT-MODE        PIC X(01).
      *       確認メッセージ返答
              05  MDCREQ-SELECT-ANSWER      PIC X(03).
      *
              05  MDCREQ-DIAGNOSIS-INFO.
      *         診療科
                07  MDCREQ-APPOINT-DEP-CODE    PIC X(2).
      *         ドクターコード
                07  MDCREQ-APPOINT-DR-CODE     PIC X(5).
      *         保険情報
                07  MDCREQ-INSURANCE-INFO.
      *           保険組合せ番号
                  09  MDCREQ-COMBINATION-NUMBER      PIC X(04).
      *           保険の種類
                  09  MDCREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *           保険者番号
                  09  MDCREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *           保険の種類名称
                  09  MDCREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *           記号
                  09  MDCREQ-MAIN-MARK               PIC X(80).
      *           番号
                  09  MDCREQ-MAIN-NUMBER             PIC X(80).
      *R03.01  枝番
                  09  MDCREQ-MAIN-EDABAN             PIC X(02).
      *           継続区分
                  09  MDCREQ-MAIN-CONTINUATION       PIC X(01).
      *           補助区分
                  09  MDCREQ-MAIN-ASSISTANCE         PIC X(01).
      *           本人家族区分
                  09  MDCREQ-MAIN-FAMILY             PIC X(01).
      *           被保険者名
                  09  MDCREQ-MAIN-POLICYHOLDER       PIC X(100).
      *           有効開始日
                  09  MDCREQ-MAIN-START-DATE         PIC X(10).
      *           有効終了日
                  09  MDCREQ-MAIN-END-DATE           PIC X(10).
      *           公費情報
                  09  MDCREQ-PUBLIC-INSURANCE-INF  OCCURS  4 TIMES.
      *             公費の種類
                    11  MDCREQ-INSURANCE-CLASS       PIC X(03).
      *             公費の種類名称
                    11  MDCREQ-INSURANCE-NAME        PIC X(20).
      *             負担者番号
                    11  MDCREQ-PROVIDER-NUMBER       PIC X(08).
      *             受給者番号
                    11  MDCREQ-RECIPIENT-NUMBER      PIC X(20).
      *             有効開始日
                    11  MDCREQ-INSURANCE-START-DATE  PIC X(10).
      *             有効終了日
                    11  MDCREQ-INSURANCE-END-DATE    PIC X(10).
      *
      *         院内院外区分
                07  MDCREQ-OUTSIDE-LASS        PIC X(05).
      *         時間外区分
                07  MDCREQ-OFFTIME             PIC X(01).
      *H31.01
      *         複数科保険診察料算定なし
                07  MDCREQ-CHENGE-AUTO         PIC X(01).
      *
      *         診療情報
                07  MDCREQ-MDC-INFO-G.
                  09  MDCREQ-MDC-INFO          OCCURS   110.
      *H31.01
      *             保険・診療科切替
                    11  MDCREQ-MDC-CHENGE-REC.
      *                 データ区分（DEP：複数診療科、COM:保険組合せ番号のレコード）
                       13  MDCREQ-MDC-DATA-KBN             PIC X(03).
      *                保険組合せ番号
                       13  MDCREQ-MDC-COMBINATION-NUMBER   PIC X(04).
      *                診療科
                       13  MDCREQ-MDC-DEP-CODE             PIC X(2).
      *                ドクター
                       13  MDCREQ-MDC-DR-CODE              PIC X(5).
      *
      *             診療種別区分
                    11  MDCREQ-MDC-CLASS             PIC X(3).
      *             診療種別名称
                    11  MDCREQ-MDC-CLASS-NAME        PIC X(40).
      *             回数
                    11  MDCREQ-MDC-CLASS-NUMBER      PIC X(03).
                    11  MDCREQ-PRESCRIPTION-INFO    OCCURS   50.
      *                 診療コード
                        13  MDCREQ-PRSCRPT-CODE          PIC X(9).
      *                 診療名称
                        13  MDCREQ-PRSCRPT-NAME          PIC X(140).
      *                 数量
                        13  MDCREQ-PRSCRPT-NUMBER        PIC X(11).
      *                 自費金額
                        13  MDCREQ-PRSCRPT-MONEY         PIC X(07).
      *                 コメント埋め込み値
                        13  MDCREQ-PRSCRPT-ATAI-G.
                            15  MDCREQ-PRSCRPT-ATAI      PIC X(10)
                                                         OCCURS   5.
      *                 フィルム分画数
                        13  MDCREQ-PRSCRPT-FILMNUM       PIC X(03).
      *                 継続コメント指示区分
                        13  MDCREQ-PRSCRPT-CONTKBN       PIC X(01).
      *                 内服種類数指示区分
                        13  MDCREQ-PRSCRPT-NAIFKBN       PIC X(01).
      *                 自動算定なし区分（在宅療養実績加算）
                        13  MDCREQ-PRSCRPT-NOAUTO        PIC X(03).
      *
      *                 自動区分
                        13  MDCREQ-PRSCRPT-AUTOKBN       PIC X(01).
      *
      *R05.11
      *        電子処方箋情報
               05  MDCREQ-PRESCRIPTIONINFO-G.
                   07  MDCREQ-PRESCRIPTIONINFO-OCC     OCCURS  15.
      *                診療科
                       09  MDCREQ-PRE-DEP-CODE         PIC X(02).
      *                保険組合せ番号
                       09  MDCREQ-PER-COMBINATION-NUMBER   PIC X(04).
      *                リフィル回数
                       09  MDCREQ-PRE-REFILL-CNT       PIC X(01).
      *                リフィル処方日数
                       09  MDCREQ-PRE-REFILL-KAISU     PIC X(03).
      *                引換番号
                       09  MDCREQ-PRE-ACCESS-CODE      PIC X(16).
