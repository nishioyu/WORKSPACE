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
       PROGRAM-ID.             ORCGKT013.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 外来まとめ入力
      *  コンポーネント名  : 受診履歴（ＫＴ０１３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/02/17    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      **
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面用共通スパ領域
           COPY    "KT01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-TIME.
               05  SYS-THH         PIC 9(02).
               05  SYS-TMM         PIC 9(02).
               05  SYS-TSS         PIC 9(02).
               05  SYS-TDD         PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-WSYSYMDH          PIC X(09).
      *
           03  WRK-WSYSYMD.
               05  WRK-WSGO         PIC X(01).
               05  WRK-WSYY         PIC 9(02).
               05  WRK-WSMM         PIC 9(02).
               05  WRK-WSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD            PIC X(09).
      *
           03  WRK-SELNUM            PIC 9(04).
           03  WRK-DOSELNUM          PIC 9(04).
      *
           03  WRK-DBPATH              PIC X(64).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-ZZ              PIC ZZZZ.
      *
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
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *****************************************************************
      *    サブプロ　領域
      *****************************************************************
      *
      *    日付変換　領域
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
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
           COPY    "ORCA61SCRAREA.INC".
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-KT01.
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
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
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-KT01        TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** ORCGKT013    START  ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KTERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "KT013"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           INITIALIZE              SPA-KT013-AREA
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-KT013-AREA
      *
      *    受診歴を検索する  ＤＯ一覧から受診履歴を確定する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KT012GMN-MAX1
               INITIALIZE                  JYURRK-REC
               MOVE    SPA-KT012-HOSPNUM     TO  JYURRK-HOSPNUM
               MOVE    SPA-KT012-PTID        TO  JYURRK-PTID
               MOVE    SPA-KT013-NYUGAIKBN   TO  JYURRK-NYUGAIKBN
      *        入院
               IF      SPA-KT013-NYUGAIKBN    =   "1"
                   MOVE    SPA-KT012NAI-RRKYMD(IDX) TO  JYURRK-SRYYMD
                   MOVE    SPA-KT012NAI-RENNUM(IDX) TO  JYURRK-RENNUM
                   MOVE    SPA-KT012NAI-RRK-SRYKA(IDX)  TO  JYURRK-SRYKA
                   MOVE    SPA-KT012NAI-RRK-HKNCOMBI(IDX)
                                               TO  JYURRK-HKNCOMBINUM
                   MOVE    "key19"             TO  WRK-DBPATH
               ELSE
                   MOVE    SPA-KT012NAI-RRK-DENPNUM(IDX)
                                               TO  JYURRK-DENPNUM
                   MOVE    "key6"              TO  WRK-DBPATH
               END-IF
      *
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    WRK-DBPATH          TO  MCP-PATHNAME
                   PERFORM 900-JYURRK-NEXT-SEC
               ELSE
                   INITIALIZE                  JYURRK-REC
                   MOVE    1               TO  FLG-JYURRK
               END-IF
      *
               PERFORM
                       UNTIL       FLG-JYURRK          =   1
      *            明細項目退避
                   PERFORM 3101-JYURRK-HEN-SEC
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    WRK-DBPATH          TO  MCP-PATHNAME
                   PERFORM 900-JYURRK-NEXT-SEC
               END-PERFORM
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
      *
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       3101-JYURRK-HEN-SEC             SECTION.
      *
           MOVE    IDX                 TO  SPA-GMN-KT013-NO      (IDX)
      *    診察
           IF      JYURRK-SRYKBN1      =   "01"
               MOVE    " ○ "          TO  SPA-GMN-KT013-SINSATSU(IDX)
           END-IF
      *    内服
           IF      JYURRK-SRYKBN2      =   "01"
               MOVE    " ○ "          TO  SPA-GMN-KT013-NAIFUKU(IDX)
           END-IF
      *    頓服
           IF      JYURRK-SRYKBN3      =   "01"
               MOVE    " ○ "          TO  SPA-GMN-KT013-TONPUKU(IDX)
           END-IF
      *    外用
           IF      JYURRK-SRYKBN4      =   "01"
               MOVE    " ○ "          TO  SPA-GMN-KT013-GAIYOU(IDX)
           END-IF
      *    注射
           IF      JYURRK-SRYKBN5      =   "01"
               MOVE    " ○ "          TO  SPA-GMN-KT013-CYUSYA(IDX)
           END-IF
      *    処置
           IF      JYURRK-SRYKBN6      =   "01"
               MOVE    " ○ "          TO  SPA-GMN-KT013-SYOCHI(IDX)
           END-IF
      *    手術
           IF      JYURRK-SRYKBN7      =   "01"
               MOVE    " ○ "          TO  SPA-GMN-KT013-OP   (IDX)
           END-IF
      *    麻酔
           IF      JYURRK-SRYKBN8      =   "01"
               MOVE    " ○ "          TO  SPA-GMN-KT013-MASUI (IDX)
           END-IF
      *    検査
           IF      JYURRK-SRYKBN9      =   "01"
               MOVE    " ○ "          TO  SPA-GMN-KT013-KENSA (IDX)
           END-IF
      *    画像
           IF      JYURRK-SRYKBN10     =   "01"
               MOVE    " ○ "          TO  SPA-GMN-KT013-GAZOU (IDX)
           END-IF
      *    その他
           IF      JYURRK-SRYKBN11     =   "01"
               MOVE    " ○ "          TO  SPA-GMN-KT013-SONOTA (IDX)
           END-IF
           .
       3101-JYURRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    確定
      *        WHEN    "CLICKED"       ALSO    "B12"
      *            PERFORM 490-KAKUTEI-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        選択
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4101-SELNUM-CHK-SEC
      *        行選択
               WHEN    "SELECT"        ALSO    "LIST"
                   PERFORM 4102-SELECT-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    KT013-SELNUM           TO  SPA-GMN-KT013-SELNUM
           IF      KT013-SELNUM           =   ZERO
               MOVE    ZERO             TO  SPA-NAI-KT013-SELNUM
           END-IF
      **** ELSE
      *        INITIALIZE                      ORCSNUMAREA
      *        MOVE    KT013-SELNUM           TO  SNUM-INX
      *        CALL    "ORCSNUM"           USING
      *                                    ORCSNUMAREA
      *        IF     (SNUM-RC         NOT =   ZERO  )   OR
      *               (SNUM-MINKBN     NOT =   SPACE )   OR
      *               (SNUM-SYOKBN     NOT =   SPACE )
      *            MOVE    "0001"              TO  SPA-ERRCD
      *            GO      TO      4101-SELNUM-CHK-EXT
      *        ELSE
      *            MOVE    SNUM-OUTNUM       TO  SPA-NAI-KT013-SELNUM
      *        END-IF
      *****END-IF
      *    MOVE    SPA-NAI-KT013-SELNUM  TO  WRK-ZZ
      *****MOVE    WRK-ZZ              TO  SPA-GMN-KT013-SELNUM
           MOVE    ZERO                TO  SPA-NAI-KT013-SELNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-KT012GMN-MAX1
              IF      SPA-KT012GMN-NO (IDX)    =   SPA-GMN-KT013-SELNUM
                  MOVE    IDX                  TO  SPA-NAI-KT013-SELNUM
                  MOVE    SPA-KT012GMN-MAX1    TO  IDX
              END-IF
           END-PERFORM
      *
           IF    ((SPA-GMN-KT013-SELNUM  NOT =   ZERO ) AND
                  (SPA-NAI-KT013-SELNUM      =   ZERO ))  OR
                 ( SPA-NAI-KT013-SELNUM  >   SPA-KT012GMN-MAX1 )
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               PERFORM 490-KAKUTEI-SEC
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       490-KAKUTEI-SEC                 SECTION.
      *
      *    IF      SPA-NAI-KT013-SELNUM       =   ZERO
      *        MOVE    "0001"          TO  SPA-ERRCD
      *    ELSE
      *        PERFORM 4101-SELNUM-CHK-SEC
      *    END-IF
      *
      *    IF      SPA-ERRCD           =   SPACE
      *        CONTINUE
      *    ELSE
      *        MOVE    "0001"          TO  SPA-ERRCD
      *        GO      TO              490-KAKUTEI-EXT
      **** END-IF
      *
      *    選択番号をＫ０９へ渡す
           MOVE    SPA-NAI-KT013-SELNUM  TO  SPA-KT012-DOSELNUM
           MOVE    SPA-GMN-KT013-SELNUM  TO  SPA-KT012-SELNUM
      *****MOVE    SPA-NAI-KT013-SELNUM  TO  WRK-ZZ
      **** MOVE    WRK-ZZ              TO  SPA-GMN-DOSELNUM
      *
           PERFORM 210-BACK
           .
       490-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日選択処理
      *****************************************************************
       4102-SELECT-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-DOSELNUM
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-KT012GMN-MAX1
              IF      KT013-SELECT(IDX)         =  "T"
                  MOVE    SPA-KT012GMN-NO (IDX)    TO  WRK-SELNUM
                  MOVE    IDX                    TO  WRK-DOSELNUM
              END-IF
           END-PERFORM
      *
           IF      WRK-DOSELNUM        >   ZERO
               MOVE    WRK-DOSELNUM        TO  SPA-NAI-KT013-SELNUM
               MOVE    WRK-SELNUM          TO  SPA-GMN-KT013-SELNUM
           END-IF
      *
           IF      SPA-NAI-KT013-SELNUM  <   01   OR
                                       >   SPA-KT012GMN-MAX1
               CONTINUE
           ELSE
      *        選択番号をＫ０９へ渡す
               MOVE    SPA-NAI-KT013-SELNUM  TO  SPA-KT012-DOSELNUM
               MOVE    SPA-GMN-KT013-SELNUM  TO  SPA-KT012-SELNUM
      *        選択後、確定終了へ
               PERFORM 210-BACK
           END-IF
      *
           .
       4102-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "KT012"               TO  SPA-SAKIPG
           MOVE    "KT013"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE   "KT012"                TO  MCP-WINDOW.
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦日付編集
      *****************************************************************
       410114-SE-WA-YMDEDIT-SEC        SECTION.
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    ZERO                TO  LNK-DAY2-YMD
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO   AND
                   STS-DAY-RC2         =   ZERO
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-WYMD
           ELSE
               MOVE    SPACE               TO  WRK-WYMD
           END-IF
           .
       410114-SE-WA-YMDEDIT-EXT.
           EXIT.
      *
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
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE   "CURRENT"    TO  MCP-PUTTYPE.
           MOVE   "KT013    "    TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  KT013
      *算定項目
           INITIALIZE                      KT013
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KT012GMN-MAX1
               MOVE    SPA-KT012GMN-NO      (IDX)   TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  KT013-NO      (IDX)
               MOVE    SPA-KT012GMN-RRKYMD  (IDX)
                                               TO  KT013-SRYYMD  (IDX)
               MOVE    SPA-GMN-KT013-SINSATSU(IDX)
                                               TO  KT013-SINSATSU(IDX)
               MOVE    SPA-GMN-KT013-NAIFUKU (IDX)
                                               TO  KT013-NAIFUKU(IDX)
               MOVE    SPA-GMN-KT013-TONPUKU (IDX)
                                               TO  KT013-TONPUKU(IDX)
               MOVE    SPA-GMN-KT013-GAIYOU  (IDX)
                                               TO  KT013-GAIYOU (IDX)
               MOVE    SPA-GMN-KT013-CYUSYA  (IDX)
                                               TO  KT013-CYUSYA (IDX)
               MOVE    SPA-GMN-KT013-SYOCHI  (IDX)
                                               TO  KT013-SYOCHI (IDX)
               MOVE    SPA-GMN-KT013-OP      (IDX)
                                               TO  KT013-OP     (IDX)
               MOVE    SPA-GMN-KT013-MASUI   (IDX)
                                               TO  KT013-MASUI  (IDX)
               MOVE    SPA-GMN-KT013-KENSA   (IDX)
                                               TO  KT013-KENSA  (IDX)
               MOVE    SPA-GMN-KT013-GAZOU   (IDX)
                                               TO  KT013-GAZOU  (IDX)
               MOVE    SPA-GMN-KT013-SONOTA  (IDX)
                                              TO  KT013-SONOTA (IDX)
           END-PERFORM
      *
           MOVE    SPA-KT012GMN-MAX1     TO  KT013-COUNT
      *
           IF      SPA-NAI-KT013-SELNUM       >   ZERO
               MOVE    SPA-GMN-KT013-SELNUM   TO  KT013-SELNUM
           END-IF
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
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    02
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
           .
       5001-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPACE       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KTERR
           MOVE    SPA-ERRMSG          TO  KTERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "KT013"             TO  SPA-MOTOPG
           MOVE    "KTERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KTERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴検索処理
      *****************************************************************
       900-JYURRK-NEXT-SEC              SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           .
       900-JYURRK-NEXT-EXT.
           EXIT.
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
