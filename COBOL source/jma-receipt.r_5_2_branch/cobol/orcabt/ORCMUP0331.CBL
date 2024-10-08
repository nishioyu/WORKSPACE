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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCMUP0331.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : 用法マスタ更新処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  23.05.24    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    入力：用法マスタ更新データ
           SELECT  IN-FILE         ASSIGN
                                   ASS-IN
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
      *    出力：エラーファイル
           SELECT  ERR-FILE        ASSIGN
                                   ASS-ERR
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-ERR.
      *    出力：プルーフファイル
           SELECT  PRF-FILE        ASSIGN
                                   ASS-PRF
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PRF.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    入力：レセプト記載マスタ更新データ
       FD  IN-FILE.
       01  IN-REC                      PIC X(1000).
      *
      *    出力：エラーファイル
       FD  ERR-FILE.
       01  ERR-YOUHOUWK-REC.
           03  ERR-YOUHOUWK-SYORI          PIC X(01).
           03  ERR-YOUHOUWK-YUKOEDYMD2     PIC X(08).
           COPY    "CPYOUHOU.INC"          REPLACING  //YOUHOU-//
                                           BY  //ERR-YOUHOUWK-//.
      *
      *    出力：プルーフファイル
       FD  PRF-FILE.
       01  PRF-REC                     PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-IN                  PIC X(256).
           03  ASS-ERR                 PIC X(256).
           03  ASS-PRF                 PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-IN                  PIC X(02).
           03  STS-ERR                 PIC X(02).
           03  STS-PRF                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-IN                  PIC 9(01).
           03  FLG-YOUHOU              PIC 9(01).
           03  FLG-PRF                 PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-INS                 PIC 9(06).
           03  CNT-UPD                 PIC 9(06).
           03  CNT-DEL                 PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-PRF                 PIC 9(06).
      *
       01  SYS-DATE.
           03  SYS-YY                  PIC 9(02).
           03  SYS-MM                  PIC 9(02).
           03  SYS-DD                  PIC 9(02).
      *
       01  SYS-TIME                    PIC 9(08).
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-ERRCD               PIC 9(01).
      *
           03  WRK-IN                  PIC X(01).
           03  WRK-HOMEDIR             PIC X(128).
      *
      *    出力ファイル領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "【PGID:ORCMUP0331.CBL】".
               05  MES-TITLE1-YY       PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-MM       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-DD       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(02) VALUE SPACE.
               05  MES-TITLE1-HOSPNUM  PIC 9(02).
      *
       01  WRK-MSG-AREA.
           03  WRK-MSG-01              PIC X(30).
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  WRK-SYORI               PIC X(01).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-CODE                PIC X(16).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-YUKOSTYMD           PIC X(08).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-YUKOEDYMD           PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-HOMEDIR           PIC X(28)  VALUE
               "/var/lib/jma-receipt/master/".
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
           COPY    "ENUM-VALUE".
           COPY    "CPORCSGETTEMP.INC".
      *
       01  YOUHOUWK-REC.
           03  YOUHOUWK-SYORI              PIC X(01).
           03  YOUHOUWK-YUKOEDYMD2         PIC X(08).
           COPY    "CPYOUHOU.INC"          REPLACING   //YOUHOU-//
                                           BY   //YOUHOUWK-//.
      *
       01  YOUHOU-REC.
           COPY    "CPYOUHOU.INC".
      *
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
           COPY    "CPMUPLINK.INC".
      *
       PROCEDURE                   DIVISION
                                   USING   LNK-AREA.
      ****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
           PERFORM 200-MAIN-SEC
                   UNTIL  (FLG-IN          =   1)
                     OR   (LNK-RC      NOT =   ZERO)
           PERFORM 300-END-SEC
           .
           EXIT    PROGRAM.
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-AREA
           MOVE    ZERO                TO  CNT-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  SPA-AREA
           MOVE    LNK-HOSPNUM         TO  SPA-HOSPNUM
      *
           ACCEPT  SYS-DATE            FROM    DATE
           COMPUTE WRK-SYY             =   SYS-YY + 2000
           MOVE    SYS-MM              TO  WRK-SMM
           MOVE    SYS-DD              TO  WRK-SDD
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    PERFORM 100-DBOPEN-SEC
           PERFORM 101-ASSIN-GET-SEC
      *
           OPEN    INPUT   IN-FILE
           IF      STS-IN          NOT =   ZERO
               MOVE    1                   TO  FLG-IN
               DISPLAY "*(ORCMUP0331)* IN-FILE OPN STS[" STS-IN "]"
           END-IF
           PERFORM 2700-OUTLST-INIT-SEC
      *
           IF      FLG-IN              =   ZERO
               PERFORM 900-IN-KENSAKU-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ファイル情報取得
      *****************************************************************
       101-ASSIN-GET-SEC           SECTION.
      *
           INITIALIZE  SGETTEMP-AREA
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
      *
           IF      SGETTEMP-MW         =   SPA-PANDA
               MOVE    CONST-HOMEDIR       TO  WRK-HOMEDIR
           ELSE
               MOVE    SGETTEMP-TEMPDIR    TO  WRK-HOMEDIR
           END-IF
      *
           MOVE    SPACE               TO  ASS-IN
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   LNK-DBFILE          DELIMITED  BY  SPACE
                   "/"                 DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                   INTO    ASS-IN
           END-STRING
      *
           MOVE    SPACE               TO  ASS-ERR
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   "orca-mstlog/"      DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                   ".ERR"              DELIMITED  BY  SIZE
                   INTO    ASS-ERR
           END-STRING
      *
           MOVE    SPACE               TO  ASS-PRF
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   "orca-mstlog/"      DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                   ".PRF"              DELIMITED  BY  SIZE
                   INTO    ASS-PRF
           END-STRING
           .
       101-ASSIN-GET-EXT.
           EXIT.
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-ERRCD
           EVALUATE    YOUHOUWK-SYORI
               WHEN    "I"
                   PERFORM 2010-INSERT-SYORI-SEC
               WHEN    "U"
                   PERFORM 2020-UPDATE-SYORI-SEC
               WHEN    "D"
                   PERFORM 2030-DELETE-SYORI-SEC
           END-EVALUATE
           PERFORM 900-IN-KENSAKU-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    追加  処理
      *****************************************************************
       2010-INSERT-SYORI-SEC       SECTION.
      *
      * *=コメントの説明
      * 使用開始日と終了日の考慮はなくキーを用法コードとした場合
      * 改定追加はなく単純追加のみの対応
      *
      *=     IF      YOUHOUWK-EDYMD      =   SPACE
      *=*        単純追加
      *=         PERFORM 2011-INSERT-SYORI-SEC
      *=     ELSE
      *=*        改定
      *=         PERFORM 2012-INSERT-SYORI-SEC
      *=     END-IF
      *=     .
      *= 2010-INSERT-SYORI-EXT.
      *=     EXIT.
      *****************************************************************
      *    単純追加  処理
      *****************************************************************
      *= 2011-INSERT-SYORI-SEC       SECTION.
      *
           INITIALIZE  YOUHOU-REC
           MOVE    YOUHOUWK-CODE       TO  YOUHOU-CODE
           MOVE    YOUHOUWK-YUKOSTYMD  TO  YOUHOU-YUKOSTYMD
           MOVE    YOUHOUWK-YUKOEDYMD  TO  YOUHOU-YUKOEDYMD
           MOVE    YOUHOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_youhou_master" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_youhou_master" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-YOUHOU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-YOUHOU
           END-IF
      *
           MOVE    "tbl_youhou_master" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    追加エラー（同一キー存在）
           IF      FLG-YOUHOU          =   ZERO
               MOVE    1                   TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           ELSE
               PERFORM 20101-YOUHOU-INS-SEC
           END-IF
           .
       2011-INSERT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    改定追加  処理
      *****************************************************************
      *= 2012-INSERT-SYORI-SEC       SECTION.
      *=*
      *=     MOVE    ZERO                TO  FLG-SYORI
      *=*
      *=     INITIALIZE  YOUHOU-REC
      *=     MOVE    YOUHOUWK-CODE       TO  YOUHOU-CODE
      *=     MOVE    YOUHOU-REC          TO  MCPDATA-REC
      *=     MOVE    "DBSELECT"          TO  MCP-FUNC
      *=     MOVE    "tbl_youhou_master" TO  MCP-TABLE
      *=     MOVE    "key"               TO  MCP-PATHNAME
      *=     PERFORM 900-ORCDBMAIN-SEC
      *=     IF      MCP-RC              =   ZERO
      *=         MOVE    "tbl_youhou_master" TO  MCP-TABLE
      *=         MOVE    "key"               TO  MCP-PATHNAME
      *=         PERFORM 910-YOUHOU-READ-SEC
      *=     ELSE
      *=         MOVE    1                   TO  FLG-YOUHOU
      *=     END-IF
      *=*
      *=     PERFORM UNTIL  (FLG-YOUHOU      =   1  )
      *=               OR   (FLG-SYORI       =   1  )
      *=         IF     (YOUHOU-YUKOSTYMD    <=  YOUHOUWK-YUKOSTYMD)
      *=           AND  (YOUHOU-YUKOEDYMD    >=  YOUHOUWK-YUKOEDYMD)
      *=*            レコード更新
      *=             MOVE    YOUHOU-CODE         TO  YOUHOU-UPCODE
      *=             MOVE    YOUHOUWK-YUKOEDYMD2 TO  YOUHOU-YUKOEDYMD
      *=             PERFORM 20101-YOUHOU-UPD-SEC
      *=*            レコード追加
      *=             PERFORM 20101-YOUHOU-INS-SEC
      *=             MOVE    1                   TO  FLG-SYORI
      *=         ELSE
      *=             MOVE    "tbl_youhou_master" TO  MCP-TABLE
      *=             MOVE    "key"               TO  MCP-PATHNAME
      *=             PERFORM 910-YOUHOU-READ-SEC
      *=         END-IF
      *=     END-PERFORM
      *=*
      *=     MOVE    "tbl_youhou_master" TO  MCP-TABLE
      *=     MOVE    "key"               TO  MCP-PATHNAME
      *=     PERFORM 900-TBL-CLOSE-SEC
      *=     IF      FLG-SYORI           =   ZERO
      *=*        レコード追加
      *=         PERFORM 20101-YOUHOU-INS-SEC
      *=     END-IF
      *=     .
      *= 2012-INSERT-SYORI-EXT.
      *=     EXIT.
      *****************************************************************
      *    更新  処理
      *****************************************************************
       2020-UPDATE-SYORI-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYORI
           INITIALIZE  YOUHOU-REC
           MOVE    YOUHOUWK-CODE       TO  YOUHOU-CODE
           MOVE    YOUHOUWK-YUKOSTYMD  TO  YOUHOU-YUKOSTYMD
           MOVE    YOUHOUWK-YUKOEDYMD  TO  YOUHOU-YUKOEDYMD
           MOVE    YOUHOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_youhou_master" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_youhou_master" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-YOUHOU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-YOUHOU
           END-IF
      *
           PERFORM UNTIL  (FLG-YOUHOU      =   1  )
                     OR   (FLG-SYORI       =   1  )
               IF     (YOUHOU-YUKOSTYMD    <=  YOUHOUWK-YUKOSTYMD)
                 AND  (YOUHOU-YUKOEDYMD    >=  YOUHOUWK-YUKOEDYMD)
                   MOVE    YOUHOU-CODE         TO  YOUHOU-UPCODE
                   MOVE    YOUHOU-YUKOSTYMD    TO  YOUHOU-UPYUKOSTYMD
                   MOVE    YOUHOU-YUKOEDYMD    TO  YOUHOU-UPYUKOEDYMD
                   MOVE    YOUHOUWK-CODE       TO  YOUHOU-CODE
                   MOVE    YOUHOUWK-BASIC-C    TO  YOUHOU-BASIC-C
                   MOVE    YOUHOUWK-BASIC-NAME TO  YOUHOU-BASIC-NAME
                   MOVE    YOUHOUWK-DETAIL-C   TO  YOUHOU-DETAIL-C
                   MOVE    YOUHOUWK-DETAIL-NAME
                                               TO  YOUHOU-DETAIL-NAME
                   MOVE    YOUHOUWK-TIMING-C   TO  YOUHOU-TIMING-C
                   MOVE    YOUHOUWK-TIMING-NAME
                                               TO  YOUHOU-TIMING-NAME
                   MOVE    YOUHOUWK-NAME       TO  YOUHOU-NAME
                   MOVE    YOUHOUWK-REFER-NO   TO  YOUHOU-REFER-NO
                   MOVE    YOUHOUWK-YUKOSTYMD  TO  YOUHOU-YUKOSTYMD
                   MOVE    YOUHOUWK-YUKOEDYMD  TO  YOUHOU-YUKOEDYMD
                   MOVE    YOUHOUWK-CODE-C     TO  YOUHOU-CODE-C
                   MOVE    YOUHOUWK-ADD-CONDITION
                                               TO  YOUHOU-ADD-CONDITION
                   MOVE    YOUHOUWK-ADD-TIMING TO  YOUHOU-ADD-TIMING
                   MOVE    YOUHOUWK-ADD-TIME   TO  YOUHOU-ADD-TIME
                   MOVE    YOUHOUWK-ADD-INTERVAL
                                               TO  YOUHOU-ADD-INTERVAL
                   MOVE    YOUHOUWK-ADD-BODY   TO  YOUHOU-ADD-BODY
                   MOVE    YOUHOUWK-KANA-NAME  TO  YOUHOU-KANA-NAME
                   MOVE    YOUHOUWK-CD-CHOZAI  TO  YOUHOU-CD-CHOZAI
                   PERFORM 20101-YOUHOU-UPD-SEC
                   MOVE    1                   TO  FLG-SYORI
               ELSE
                   MOVE    "tbl_youhou_master" TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-YOUHOU-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_youhou_master" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
           IF      FLG-SYORI           =   ZERO
               MOVE    5                   TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       2020-UPDATE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    削除  処理
      *****************************************************************
       2030-DELETE-SYORI-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYORI
           INITIALIZE  YOUHOU-REC
           MOVE    YOUHOUWK-CODE       TO  YOUHOU-CODE
           MOVE    YOUHOUWK-YUKOSTYMD  TO  YOUHOU-YUKOSTYMD
           MOVE    YOUHOUWK-YUKOEDYMD  TO  YOUHOU-YUKOEDYMD
           MOVE    YOUHOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_youhou_master" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_youhou_master" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-YOUHOU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-YOUHOU
           END-IF
      *
           PERFORM UNTIL  (FLG-YOUHOU      =   1  )
                     OR   (FLG-SYORI       =   1  )
               IF     (YOUHOU-YUKOSTYMD    <=  YOUHOUWK-YUKOSTYMD)
                 AND  (YOUHOU-YUKOEDYMD    >=  YOUHOUWK-YUKOEDYMD)
                   PERFORM 20101-YOUHOU-DEL-SEC
                   MOVE    1                   TO  FLG-SYORI
               ELSE
                   MOVE    "tbl_youhou_master" TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-YOUHOU-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_youhou_master" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
           IF      FLG-SYORI           =   ZERO
               MOVE    6                   TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       2030-DELETE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加  処理
      *****************************************************************
       20101-YOUHOU-INS-SEC        SECTION.
      *
           INITIALIZE  YOUHOU-REC
           MOVE    YOUHOUWK-REC(10:)   TO  YOUHOU-REC
           MOVE    LNK-MNTYMD          TO  YOUHOU-CREYMD
           MOVE    LNK-MNTHMS          TO  YOUHOU-CREHMS
           MOVE    LNK-MNTYMD          TO  YOUHOU-UPYMD
           MOVE    LNK-MNTHMS          TO  YOUHOU-UPHMS
           MOVE    YOUHOU-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_youhou_master" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  LNK-RC
               MOVE    2                   TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           IF      WRK-ERRCD           =   ZERO
               ADD     1                   TO  CNT-INS
           END-IF
           .
       20101-YOUHOU-INS-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新  処理
      *****************************************************************
       20101-YOUHOU-UPD-SEC        SECTION.
      *
           MOVE    LNK-MNTYMD          TO  YOUHOU-UPYMD
           MOVE    LNK-MNTHMS          TO  YOUHOU-UPHMS
           MOVE    YOUHOU-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_youhou_master" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    2                   TO  LNK-RC
               MOVE    3                   TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           IF      WRK-ERRCD           =   ZERO
               ADD     1                   TO  CNT-UPD
           END-IF
           .
       20101-YOUHOU-UPD-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除  処理
      *****************************************************************
       20101-YOUHOU-DEL-SEC        SECTION.
      *
           MOVE    YOUHOU-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_youhou_master" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    3                   TO  LNK-RC
               MOVE    4                   TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           IF      WRK-ERRCD           =   ZERO
               ADD     1                   TO  CNT-DEL
           END-IF
           .
       20101-YOUHOU-DEL-EXT.
           EXIT.
      *****************************************************************
      *    エラー処理
      *****************************************************************
       2040-ERR-SYORI-SEC          SECTION.
      *
           MOVE    YOUHOUWK-REC        TO  ERR-YOUHOUWK-REC
           WRITE   ERR-YOUHOUWK-REC
           ADD     1                   TO  CNT-ERR
           PERFORM 20401-PRF-OUT-SEC
           .
       2040-ERR-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ出力処理
      *****************************************************************
       20401-PRF-OUT-SEC           SECTION.
      *
           MOVE    YOUHOUWK-SYORI      TO  WRK-SYORI
           MOVE    YOUHOUWK-CODE       TO  WRK-CODE
           MOVE    YOUHOUWK-YUKOSTYMD  TO  WRK-YUKOSTYMD
           MOVE    YOUHOUWK-YUKOEDYMD  TO  WRK-YUKOEDYMD
      *
           EVALUATE    WRK-ERRCD
               WHEN    1
                   MOVE  "用法マスタ登録済み"  TO  WRK-MSG-01
               WHEN    2
                   MOVE  "追加エラー"          TO  WRK-MSG-01
               WHEN    3
                   MOVE  "更新エラー"          TO  WRK-MSG-01
               WHEN    4
                   MOVE  "削除エラー"          TO  WRK-MSG-01
               WHEN    5
                   MOVE  "更新対象なし"        TO  WRK-MSG-01
               WHEN    6
                   MOVE  "削除対象なし"        TO  WRK-MSG-01
           END-EVALUATE
      *
           MOVE    WRK-MSG-AREA        TO  PRF-REC
           WRITE   PRF-REC
           ADD     1                   TO  CNT-PRF
           .
       20401-PRF-OUT-EXT.
           EXIT.
      *****************************************************************
      *    出力ファイル　初期処理
      *****************************************************************
       2700-OUTLST-INIT-SEC        SECTION.
      *
           OPEN    OUTPUT  ERR-FILE
           IF      STS-ERR         NOT =   ZERO
               DISPLAY "*(ORCMUP0331)* ERR FILE OPN STS[" STS-ERR "]"
               GO  TO  2700-OUTLST-INIT-EXT
           END-IF
      *
           OPEN    OUTPUT  PRF-FILE
           IF      STS-PRF         NOT =   ZERO
               DISPLAY "*(ORCMUP0331)* PRF FILE OPN STS[" STS-PRF "]"
               GO  TO  2700-OUTLST-INIT-EXT
           END-IF
      *
           MOVE    WRK-SYY             TO  MES-TITLE1-YY
           MOVE    SYS-MM              TO  MES-TITLE1-MM
           MOVE    SYS-DD              TO  MES-TITLE1-DD
           MOVE    SPA-HOSPNUM         TO  MES-TITLE1-HOSPNUM
           MOVE    MES-TITLE1          TO  PRF-REC
           WRITE   PRF-REC
           .
       2700-OUTLST-INIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    PERFORM 900-DBDISCONNECT-SEC
      *
           CLOSE   IN-FILE
           CLOSE   ERR-FILE
           CLOSE   PRF-FILE
      *
           IF      LNK-RC              =   ZERO
               IF      CNT-IN          NOT =   LNK-COUNT
                   MOVE    99                  TO  LNK-RC
                   INITIALIZE  JOBKANRI-REC
                   MOVE    1                   TO  JOB-HOSPNUM
                   MOVE    "tbl_jobkanri"      TO  MCP-TABLE
                   MOVE    "abort"             TO  MCP-PATHNAME
                   MOVE    JOBKANRI-REC        TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
               END-IF
           END-IF
      *
           DISPLAY "*(ORCMUP0331)* INPUT  /I   CNT[" CNT-IN  "]"
           DISPLAY "*(ORCMUP0331)*  INSERT     CNT[" CNT-INS "]"
           DISPLAY "*(ORCMUP0331)*  UPDATE     CNT[" CNT-UPD "]"
           DISPLAY "*(ORCMUP0331)*  DELETE     CNT[" CNT-DEL "]"
           DISPLAY "*(ORCMUP0331)* ERR    /O   CNT[" CNT-ERR "]"
           DISPLAY "*(ORCMUP0331)* PRF    /O   CNT[" CNT-PRF "]"
           DISPLAY "*(ORCMUP0331)* RC             [" LNK-RC "]"
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    更新データ読込
      *****************************************************************
       900-IN-KENSAKU-SEC          SECTION.
      *
           READ    IN-FILE
               AT  END
                   MOVE    1                   TO  FLG-IN
               NOT AT  END
                   MOVE    ZERO                TO  FLG-IN
                   ADD     1                   TO  CNT-IN
           END-READ
      *
           IF      FLG-IN              =   ZERO
               MOVE    SPACE               TO  YOUHOUWK-REC
               MOVE    IN-REC              TO  YOUHOUWK-REC
           END-IF
           .
       900-IN-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    用法読込
      *****************************************************************
       910-YOUHOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  YOUHOU-REC
               MOVE    ZERO                TO  FLG-YOUHOU
           ELSE
               INITIALIZE  YOUHOU-REC
               MOVE    1                   TO  FLG-YOUHOU
           END-IF
           .
       910-YOUHOU-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-TBL-CLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-TBL-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
