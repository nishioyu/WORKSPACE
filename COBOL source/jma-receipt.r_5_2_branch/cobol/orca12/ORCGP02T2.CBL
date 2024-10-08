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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGP02T2.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 保険証限度額認定証　認証日履歴画面（Ｐ０２Ｔ２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/01/XX    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTCONF          PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-RENNUM              PIC 9(03).
           03  WRK-ZZ9                 PIC ZZ9.
      *
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-LIST-ROW            PIC S9(09).
           03  WRK-LIST-INIT           PIC 9(01).
      *
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者個別設定テーブル
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA12SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面編集後、表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P02T2"             TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *
           EVALUATE    SPA-MOTOPG
      *        確認メッセージ
               WHEN    "PID1"
                   PERFORM 3001-PID1-SYORI-SEC
      *
               WHEN    OTHER
      *        ＳＰＡ編集
                   PERFORM 300-SPA-INIT-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       300-SPA-INIT-SEC                SECTION.
      *
           INITIALIZE                  SPA-P02T2-ONSGENDO-LST
      *
      *    限度額承認履歴を検索
           INITIALIZE                  PTCONF-REC
           MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTCONF-PTID
           MOVE    "ONSGENDO%"         TO  PTCONF-CKEY
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptconf"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 930-PTCONF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTCONF
               INITIALIZE                  PTCONF-REC
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  SPA-P02T2-MAX
           PERFORM UNTIL       FLG-PTCONF      =   1
     *                  OR    (IDX             =   99 )
               ADD     1                   TO  IDX
               MOVE    PTCONF-CDATA(1:8)   TO  WRK-SYMD
               PERFORM 4001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P02T2-ONSGENDO-YMD
                                                                 (IDX)
               ADD     1                   TO  SPA-P02T2-MAX
      * 
               MOVE    "tbl_ptconf"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 930-PTCONF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       300-SPA-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-PIDCD
           MOVE    SPACE               TO  SPA-PID1-FLG
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  P02T2
           INITIALIZE                      P02T2
      *
           MOVE    SPA-P02T2-MAX       TO  P02T2-ONSGENDOLST-COUNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-P02T2-MAX )
                MOVE    IDX                TO  WRK-ZZ9
                MOVE    WRK-ZZ9            TO  P02T2-RENNUM (IDX)
                MOVE    SPA-P02T2-ONSGENDO-YMD (IDX)
                                           TO  P02T2-ONSGENDO (IDX)
           END-PERFORM
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-P02T2-CUR        =   ZERO
               MOVE    1                   TO  SPA-P02T2-CUR
           END-IF
           EVALUATE    SPA-P02T2-CUR
               WHEN    01
                   MOVE    "B01"              TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK-SYORI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC          SECTION.
      *
           PERFORM 210-BACK
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P02T2"             TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "P02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           MOVE    ZERO                TO  SPA-FLG-END
      **** MOVE    "P02"               TO  SPA-P02-GAMEN
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       4001-SEIWA-YMDEDIT-SEC           SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    SPACE
                   MOVE    SPACE           TO  WRK-HENYMD
               WHEN    ZERO
                   MOVE    "0000000"       TO  WRK-HENYMD
               WHEN    "99999999"
                   MOVE    "9999999"       TO  WRK-HENYMD
               WHEN    OTHER
      *
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   IF      STS-DAY-RC1         =   ZERO
                       MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
                   ELSE
                       MOVE    SPACE               TO  WRK-HENYMD
                   END-IF
           END-EVALUATE
           .
       4001-SEIWA-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "P02T2"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号エラー"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02T2"             TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    患者個別設定検索処理
      *****************************************************************
       930-PTCONF-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTCONF-REC
               MOVE    ZERO                TO  FLG-PTCONF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1                   TO  FLG-PTCONF
           END-IF
           .
       930-PTCONF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
