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
      ******************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORAPION009R1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 予約患者一括資格確認照会処理
      *                     （資格確認結果登録処理）
      *  管理者            :
      *  作成日付    作業者        記述
      *  21/11/30    ORCAMO        新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ******************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-YYK                 PIC 9(01).
           03  FLG-JOBKANRI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDW                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDXX                    PIC 9(04).
           03  IDYY                    PIC 9(04).
           03  IDZZ                    PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
            03  WRK-CNT-X.
                05  WRK-CNT             PIC 9(04).
            03  WRK-PTID                PIC 9(10).
            03  WRK-UUID                PIC X(36).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *    エラーコード
           03  WRK-ERRCD               PIC X(03).
           03  WRK-ERRMSG              PIC X(100).
      *    全角変換用 
           03  WRK-KANACHK-MAE-INPUT   PIC X(5000).
           03  WRK-KANACHK-OUT-INPUT   PIC X(5000).
           03  WRK-KANACHK-LEN         PIC 9(04).
      *
       01  WRK-QUAAPPREQ2-AREA.     
           03  WRK-QUAAPPREQ2-SHO-KIGO    PIC X(80).
           03  WRK-QUAAPPREQ2-SHO-NUM     PIC X(80).
           03  WRK-QUAAPPREQ2-RESULT-AREA.
               05  WRK-QUAAPPREQ2-RES-KIGO
                                       PIC X(80).
               05  WRK-QUAAPPREQ2-RES-NUM PIC X(80).
               05  WRK-QUAAPPREQ2-RES-HIHKNJANAME
                                       PIC X(200).
               05  WRK-QUAAPPREQ2-RES-NAME
                                       PIC X(200).
               05  WRK-QUAAPPREQ2-RES-NAME-ETC
                                       PIC X(200).
               05  WRK-QUAAPPREQ2-RES-KANANAME
                                       PIC X(200).
               05  WRK-QUAAPPREQ2-RES-KANANAME1
                                       PIC X(200).
               05  WRK-QUAAPPREQ2-RES-KANANAME2
                                       PIC X(200).
               05  WRK-QUAAPPREQ2-RES-KANANAME-ETC
                                       PIC X(200).
               05  WRK-QUAAPPREQ2-RES-ADRS
                                       PIC X(500).
               05  WRK-QUAAPPREQ2-HKNJANUM-NAME
                                       PIC X(200).
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                       PIC X(08)   VALUE   "yoyaku01".
           03  WRK-CONS-JOB-JOBID      PIC 9(07)   VALUE   ZERO.
           03  WRK-CONS-RES-MAX        PIC 9(04)   VALUE   300.
           03  WRK-CONS-RESULT-MAX     PIC 9(04)   VALUE   1.
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
      *     COPY    "CPORCSPTID.INC".
      *    患者番号変換サブ
      *     COPY    "CPORCSPTNUM.INC".
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *    拡張漢字チェックサブ
           COPY    "CPORCSKANACHK2.INC".
           COPY    "CPORCSCOMMON.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEQUAAPPREQ2.INC".
      *    API XML書き込み用定義体
           COPY    "CPONLINEQUAAPPRES1.INC".
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
           COPY    "MCPDATA2.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK1001.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    予約
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
       *
      *    資格確認テーブル
       01  ONSKAKU-REC.       
           COPY    "CPONSHI-KAKU.INC".
     *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA14SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "****************************"
           DISPLAY   "*   onlinequaapp2  start   *"
           DISPLAY   "****************************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "999"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST09-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "****************************"
           DISPLAY   "*   onlinequaapp2    end   *"
           DISPLAY   "****************************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  XML-ONLINEQUAAPPRES1
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  QUAAPPRES1-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  QUAAPPRES1-INFORMATION-TIME
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "999"               TO  WRK-ERRCD
               GO  TO  100-INIT-EXT
           END-IF
      * 
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "E89"            TO  WRK-ERRCD
               GO  TO  100-INIT-EXT
           END-IF
            .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    SPACE           TO  WRK-ERRCD
      *
      *    XML情報取り出し
           PERFORM 1000-RGTXMLREAD-SEC
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    入力項目チェック処理
           PERFORM 2001-INPUT-CHK-SEC
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    資格確認結果登録処理
           PERFORM 2003-JOBKANRI-UPDATE-SEC
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *     
           IF      QUAAPPREQ2-RESULT-KBN
                                   =   "1"
      *        資格確認レコード作成処理
               PERFORM 2002-SIKAKU-KAKUNIN-INSERT-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           IF      APILST09-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONLINEQUAAPPREQ2
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinequaappreq2"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST09-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "onlinequaappreq2"  TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ONLINEQUAAPPREQ2
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "E98"             TO  WRK-ERRCD
           END-IF
      *
           .
       1000-RGTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-RGTXMLMAKE-SEC   SECTION.
      *
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "000"               TO  QUAAPPRES1-API-RESULT
               MOVE    "処理終了"          TO  QUAAPPRES1-API-RESULT-MSG
           ELSE
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  QUAAPPRES1-API-RESULT
               MOVE    WRK-ERRMSG      TO  QUAAPPRES1-API-RESULT-MSG
           END-IF
      *
           IF      APILST09-BODY       NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "xml_onlinequaappres1"
                                       TO  MCP-TABLE
           MOVE    1                   TO  QUAAPPRES1-MODE
           MOVE    "onlinequaappres1"  TO  QUAAPPRES1-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ONLINEQUAAPPRES1
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  QUAAPPRES1-OBJECT    TO  APILST09-BODY
               MOVE  "application/xml" TO  APILST09-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       300-RGTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
           IF      QUAAPPREQ2-DATAREQ
                               =   SPACE 
               MOVE    "E15"           TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           IF      QUAAPPREQ2-RESULT-KBN
                               =   "1"    OR  "9" 
                CONTINUE
           ELSE
               MOVE    "E12"       TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     資格確認レコード作成処理
      *****************************************************************
       2002-SIKAKU-KAKUNIN-INSERT-SEC    SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-CONS-RES-MAX
                   OR      QUAAPPREQ2-SHO-SIKIBETU (IDXX)
                                   =       SPACE
               INITIALIZE              WRK-PTID
                                       WRK-UUID
               UNSTRING    QUAAPPREQ2-SHO-SIKIBETU (IDXX)
                                       DELIMITED   BY  ","
                                       INTO    WRK-PTID
                                               WRK-UUID
               END-UNSTRING
      *
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       WRK-CONS-RESULT-MAX
      *            全角変換処理
                   PERFORM 20021-KANACHK-HENSYU-SEC
      *
                   INITIALIZE                  ONSKAKU-REC
                   MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
                   MOVE    WRK-UUID        TO  ONSKAKU-TBL-UUID
                   MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
                    MOVE    WRK-PTID        TO  ONSKAKU-PTID
                   MOVE    "1"             TO  ONSKAKU-SET-PTID
                   MOVE    JOB-SRYYMD      TO  ONSKAKU-KAKUNINYMD
                   MOVE    "1"             TO  ONSKAKU-ORCA-SHOKAI-KBN
                   MOVE    SMCNDATE-YMD    TO  ONSKAKU-KAKU-KAKUNINYMD
                   MOVE    SMCNDATE-HMS    TO  ONSKAKU-KAKU-KAKUNINHMS 
                   MOVE    QUAAPPREQ2-SHORI-TIME
                                           TO  ONSKAKU-SHORI-TIME 
                   MOVE    QUAAPPREQ2-HOSPCD  TO  ONSKAKU-HOSPCD
                   MOVE    QUAAPPREQ2-FILE-SIKIBETU
                                           TO  ONSKAKU-FILE-SIKIBETU
      *************MOVE    QUAAPPREQ2-SHOKAI-KBN
                   MOVE    "2"             TO  ONSKAKU-SHOKAI-KBN
                   MOVE    QUAAPPREQ2-RESULT-KBN
                                           TO  ONSKAKU-RESULT-KBN
                   MOVE    QUAAPPREQ2-ERR-CODE
                                           TO  ONSKAKU-ERR-CODE
                   MOVE    QUAAPPREQ2-ERR-MSG TO  ONSKAKU-ERR-MSG
                   MOVE    QUAAPPREQ2-CODE-TYPE
                                       TO  ONSKAKU-CODE-TYPE
      *                             
                   MOVE    QUAAPPREQ2-SHO-HKNJANUM (IDXX)
                                           TO  ONSKAKU-SHO-HKNJANUM
                   MOVE    QUAAPPREQ2-SHO-KIGO     (IDXX)
                                           TO  ONSKAKU-SHO-KIGO
                   MOVE    QUAAPPREQ2-SHO-NUM      (IDXX)
                                           TO  ONSKAKU-SHO-NUM
                   MOVE    QUAAPPREQ2-SHO-EDABAN   (IDXX)
                                           TO  ONSKAKU-SHO-EDABAN
                   MOVE    QUAAPPREQ2-SHO-BIRTHDAY (IDXX)
                                           TO  ONSKAKU-SHO-BIRTHDAY
      **           MOVE    QUAAPPREQ2-SHO-DOUIFLG
      **                                   TO  ONSKAKU-SHO-DOUIFLG
                   MOVE    QUAAPPREQ2-RESULT       (IDXX)
                                           TO  ONSKAKU-RESULT
                   MOVE    QUAAPPREQ2-RESULT-CODE  (IDXX)
                                           TO  ONSKAKU-RESULT-CODE
                   MOVE    QUAAPPREQ2-RESULT-MSG   (IDXX)
                                           TO  ONSKAKU-RESULT-MSG
                   MOVE    QUAAPPREQ2-SIKAKU-YUKO  (IDXX)
                                           TO  ONSKAKU-SIKAKU-YUKO
      *
                   MOVE    QUAAPPREQ2-CARD-CLASS   (IDXX IDYY) 
                                           TO  ONSKAKU-CARD-CLASS 
                   MOVE    QUAAPPREQ2-RES-HKNJANUM (IDXX IDYY)
                                           TO  ONSKAKU-RES-HKNJANUM
                   MOVE    WRK-QUAAPPREQ2-RES-KIGO
                                           TO  ONSKAKU-RES-KIGO
                   MOVE    WRK-QUAAPPREQ2-RES-NUM
                                           TO  ONSKAKU-RES-NUM 
                   MOVE    QUAAPPREQ2-RES-EDABAN (IDXX IDYY) 
                                           TO  ONSKAKU-RES-EDABAN 
                   MOVE    QUAAPPREQ2-RES-HONKZKKBN (IDXX IDYY) 
                                           TO  ONSKAKU-RES-HONKZKKBN 
                   MOVE    WRK-QUAAPPREQ2-RES-HIHKNJANAME
                                           TO  ONSKAKU-RES-HIHKNJANAME 
                   MOVE    WRK-QUAAPPREQ2-RES-NAME
                                           TO  ONSKAKU-RES-NAME 
                   MOVE    WRK-QUAAPPREQ2-RES-NAME-ETC
                                           TO  ONSKAKU-RES-NAME-ETC 
                   MOVE    WRK-QUAAPPREQ2-RES-KANANAME
                                           TO  ONSKAKU-RES-KANANAME 
                   MOVE    WRK-QUAAPPREQ2-RES-KANANAME-ETC
                                           TO  ONSKAKU-RES-KANANAME-ETC
                   MOVE    QUAAPPREQ2-RES-SEX1 (IDXX IDYY)
                                           TO  ONSKAKU-RES-SEX1 
                   MOVE    QUAAPPREQ2-RES-SEX2 (IDXX IDYY)
                                           TO  ONSKAKU-RES-SEX2 
                   MOVE    QUAAPPREQ2-RES-BIRTHDAY (IDXX IDYY)
                                           TO  ONSKAKU-RES-BIRTHDAY 
                   MOVE    WRK-QUAAPPREQ2-RES-ADRS
                                           TO  ONSKAKU-RES-ADRS 
                   MOVE    QUAAPPREQ2-RES-POST (IDXX IDYY)
                                           TO  ONSKAKU-RES-POST 
                   MOVE    QUAAPPREQ2-SKKGETYMD (IDXX IDYY)
                                           TO  ONSKAKU-SKKGETYMD 
                   MOVE    QUAAPPREQ2-TEKSTYMD (IDXX IDYY)
                                           TO  ONSKAKU-TEKSTYMD 
                   MOVE    QUAAPPREQ2-TEKEDYMD (IDXX IDYY)
                                           TO  ONSKAKU-TEKEDYMD 
                   MOVE    QUAAPPREQ2-FTNRATE (IDXX IDYY)
                                           TO  ONSKAKU-FTNRATE 
                   MOVE    QUAAPPREQ2-MISYUGAKU (IDXX IDYY)
                                           TO  ONSKAKU-MISYUGAKU 
                   MOVE    QUAAPPREQ2-LOSS-JIYU  (IDXX IDYY)
                                           TO  ONSKAKU-LOSS-JIYU 
                   MOVE    WRK-QUAAPPREQ2-HKNJANUM-NAME
                                           TO  ONSKAKU-HKNJANUM-NAME 
                   MOVE    QUAAPPREQ2-ELDER-SKKGETYMD (IDXX IDYY)
                                           TO  ONSKAKU-ELDER-SKKGETYMD 
                   MOVE    QUAAPPREQ2-ELDER-TEKSTYMD (IDXX IDYY)
                                           TO  ONSKAKU-ELDER-TEKSTYMD 
                   MOVE    QUAAPPREQ2-ELDER-TEKEDYMD (IDXX IDYY)
                                           TO  ONSKAKU-ELDER-TEKEDYMD 
                   MOVE    QUAAPPREQ2-ELDER-FTNRATE (IDXX IDYY)
                                           TO  ONSKAKU-ELDER-FTNRATE 
                   MOVE    QUAAPPREQ2-GENDO-SHOKBN (IDXX)
                                           TO  ONSKAKU-GENDO-SHOKBN 
                   MOVE    QUAAPPREQ2-GENDO-TEKKBN (IDXX)
                                           TO  ONSKAKU-GENDO-TEKKBN 
                   MOVE    QUAAPPREQ2-FILE-SIKIBETU
                                           TO  ONSKAKU-SIKIBETU 
                   MOVE    QUAAPPREQ2-SHOKAI-NUM (IDXX IDYY)
                                           TO  ONSKAKU-SHOKAI-NUM
      *                             
                   MOVE    SPA-OPID        TO  ONSKAKU-OPID
                   MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
                   MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
                   MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
                   MOVE    SPA-OPID        TO  ONSKAKU-OPID
      *
                   MOVE    ONSKAKU-REC     TO  MCPDATA-REC
                   MOVE    "DBINSERT"      TO  MCP-FUNC
                   MOVE    "tbl_onshi_kaku"
                                           TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                    IF      MCP-RC      NOT =   ZERO 
                        MOVE   "E??"             TO  WRK-ERRCD
                        DISPLAY "TBL_ONSHI_KAKU INSERT ERR " MCP-RC 
                   END-IF
               END-PERFORM        
           END-PERFORM
      *
           .        
       2002-SIKAKU-KAKUNIN-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       20021-KANACHK-HENSYU-SEC    SECTION.
      *
           INITIALIZE                  WRK-QUAAPPREQ2-RESULT-AREA
      *
           MOVE    QUAAPPREQ2-RES-KIGO (IDXX IDYY)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-RES-KIGO
           MOVE    QUAAPPREQ2-RES-NUM (IDXX IDYY)
                                   TO    WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-RES-NUM
           MOVE    QUAAPPREQ2-RES-HIHKNJANAME (IDXX IDYY)
                                   TO  WRK-KANACHK-MAE-INPUT
               MOVE    200         TO  WRK-KANACHK-LEN
               PERFORM 801-KANACHK2-SEC                       
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-RES-HIHKNJANAME
           MOVE    QUAAPPREQ2-RES-NAME (IDXX IDYY)
                                   TO  WRK-KANACHK-MAE-INPUT
               MOVE    200         TO  WRK-KANACHK-LEN
               PERFORM 801-KANACHK2-SEC                       
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-RES-NAME
            MOVE    QUAAPPREQ2-RES-NAME-ETC (IDXX IDYY)
                                   TO  WRK-KANACHK-MAE-INPUT
               MOVE    200         TO  WRK-KANACHK-LEN
               PERFORM 801-KANACHK2-SEC                       
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-RES-NAME-ETC
           MOVE    QUAAPPREQ2-RES-KANANAME (IDXX IDYY)
                                   TO    WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-RES-KANANAME
           MOVE    QUAAPPREQ2-RES-KANANAME-ETC (IDXX IDYY)
                                   TO    WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-RES-KANANAME-ETC
            MOVE    QUAAPPREQ2-RES-ADRS (IDXX IDYY)
                                   TO    WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    1               TO  IDZ
           PERFORM VARYING IDZZ    FROM    1  BY  2
                   UNTIL   IDZZ    >       250
                   OR      WRK-KANACHK-OUT-INPUT (IDZZ:)
                                   =       SPACE
               IF      WRK-KANACHK-OUT-INPUT (IDZZ:2)
                                   NOT =   "　"
                   MOVE    WRK-KANACHK-OUT-INPUT (IDZZ:2)
                                   TO  WRK-QUAAPPREQ2-RES-ADRS (IDZ:2)
                   ADD     2       TO  IDZ
               END-IF
           END-PERFORM
           MOVE    QUAAPPREQ2-HKNJANUM-NAME (IDXX IDYY)
                                   TO    WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-HKNJANUM-NAME
      *
           .        
       20021-KANACHK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理更新処理
      *****************************************************************
       2003-JOBKANRI-UPDATE-SEC  SECTION.
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    QUAAPPREQ2-FILE-SIKIBETU (1:36)
                                   TO  JOB-KARTE-UID
           PERFORM 900-JOBKANRI-READ-SEC
           IF      FLG-JOBKANRI    =   1
               MOVE    "E14"           TO  WRK-ERRCD
           ELSE
               MOVE    QUAAPPREQ2-RESULT-KBN
                                       TO  JOB-SYOKBN
               INITIALIZE                  ORCSNUMAREA
               MOVE    QUAAPPREQ2-ALL-CNT
                                       TO  SNUM-INX
               CALL    "ORCSNUM"       USING
                                       ORCSNUMAREA
               IF      SNUM-RC         =   ZERO
                   MOVE    SNUM-OUTNUM     TO  JOB-UPDCNT
               END-IF    
               INITIALIZE                  ORCSNUMAREA
               MOVE    QUAAPPREQ2-OK-CNT
                                       TO  SNUM-INX
               CALL    "ORCSNUM"       USING
                                       ORCSNUMAREA
               IF      SNUM-RC         =   ZERO
                   MOVE    SNUM-OUTNUM     TO  JOB-UPDCNT1
               END-IF
               INITIALIZE                  ORCSNUMAREA
               MOVE    QUAAPPREQ2-ERR-CNT
                                       TO  SNUM-INX
               CALL    "ORCSNUM"       USING
                                       ORCSNUMAREA
               IF      SNUM-RC         =   ZERO
                   MOVE    SNUM-OUTNUM     TO  JOB-UPDCNT2
               END-IF
               MOVE    JOBKANRI-REC    TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_jobkanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          >   ZERO
                   DISPLAY "JOBKANRI UPDATE ERR=" MCP-RC
                   MOVE    "E??"           TO  WRK-ERRCD
               END-IF
           END-IF
           .
       2003-JOBKANRI-UPDATE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     全角変換処理
      *****************************************************************
       800-KANACHK-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-KANACHK-OUT-INPUT
           IF      WRK-KANACHK-MAE-INPUT
                               NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC      =   2    )
                   CONTINUE
               ELSE    
                   MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                           TO  WRK-KANACHK-OUT-INPUT
               END-IF
           END-IF
      *
           .
       800-KANACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     全角変換処理（拡張漢字対応）
      *****************************************************************
       801-KANACHK2-SEC           SECTION.
      *
           IF     SPA-ENCODING         =   2
      *        拡張漢字対応
               MOVE    SPACE           TO  WRK-KANACHK-OUT-INPUT
               IF      WRK-KANACHK-MAE-INPUT
                                   NOT =   SPACE
                   INITIALIZE                  ORCSKANACHK2AREA
                   MOVE    "2"             TO  KANACHK2-SYORI
                   MOVE    WRK-KANACHK-LEN TO  KANACHK2-MAX-CNT
                   MOVE    WRK-KANACHK-MAE-INPUT
                                           TO  KANACHK2-MAE-INPUT
                   CALL    "ORCSKANACHK2"  USING   ORCSKANACHK2AREA
                   IF      KANACHK2-RC NOT =   ZERO
                       MOVE    KANACHK2-MAE-INPUT
                                               TO  WRK-KANACHK-OUT-INPUT
                   ELSE
                       MOVE    KANACHK2-OUT-INPUT
                                               TO  WRK-KANACHK-OUT-INPUT
                   END-IF
               END-IF
           ELSE
      *        拡張漢字不可
               PERFORM 800-KANACHK-SEC
           END-IF    
      *
           .
       801-KANACHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           INITIALIZE                  WRK-HEN-DATE
           MOVE    WRK-SYY             TO  WRK-HEN-YY
           MOVE    WRK-SMM             TO  WRK-HEN-MM
           MOVE    WRK-SDD             TO  WRK-HEN-DD
           MOVE    "-"                 TO  WRK-HEN-H1
           MOVE    "-"                 TO  WRK-HEN-H2
           IF      WRK-SYMD            =   "99999999"
               MOVE    "12"                TO  WRK-HEN-MM
               MOVE    "31"                TO  WRK-HEN-DD
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           EVALUATE    WRK-ERRCD
               WHEN    "E11"
                   MOVE    "最終予約番号の設定に誤りがあります"
                                                   TO  WRK-ERRMSG
               WHEN    "E12"
                   MOVE    "処理区分の設定に誤りがあります"
                                                   TO  WRK-ERRMSG
               WHEN    "E13"
                   MOVE    "ＵＩＤが未設定です"    TO  WRK-ERRMSG
               WHEN    "E14"
                   MOVE    "ＵＩＤに一致する処理がありません"
                                                   TO  WRK-ERRMSG
               WHEN    "E15"
                   MOVE    "リクエストが空です。"  TO  WRK-ERRMSG
      *        共通エラー
               WHEN    "E89"
      *            ORCSCOMMON のエラー
                   EVALUATE    SPA-ERRCD
                       WHEN    "1001"
                           MOVE    "職員情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1002"
                           MOVE    "医療機関情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1003"
                           MOVE    "システム日付が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1005"
                           MOVE    "患者番号構成情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1015"
                           STRING  "グループ医療機関が不整合です。"
                               "処理を終了して下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-ERRMSG
                           END-STRING
                       WHEN    OTHER
                           MOVE    "システム項目が設定できません。"
                                                   TO  WRK-ERRMSG
                   END-EVALUATE
               WHEN    "E90"
                   MOVE    "他端末で使用中です。"  TO  WRK-ERRMSG
               WHEN    "E91"
                   MOVE    "リクエスト番号が不正です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E97"
                   MOVE   "送信内容に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E98"
                   MOVE   "送信内容の読込ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E99"
                   MOVE    "ユーザＩＤが未登録です。"
                                                   TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込処理
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理マスター読込処理
      *****************************************************************
       900-JOBKANRI-READ-SEC       SECTION.
      * 
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  JOBKANRI-REC
                   MOVE    ZERO                TO  FLG-JOBKANRI
               ELSE
                   INITIALIZE                      JOBKANRI-REC
                   MOVE    1                   TO  FLG-JOBKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-JOBKANRI
           END-IF
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-JOBKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    予約マスター読込処理
      *****************************************************************
       900-YYK-READ-N-SEC          SECTION.
      * 
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key13"         TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  YYK-REC
               MOVE    ZERO                TO  FLG-YYK
           ELSE
               INITIALIZE                      YYK-REC
               MOVE    1                   TO  FLG-YYK
           END-IF
           .
       900-YYK-READ-N-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者保険マスター読込処理
      ******************************************************************
       900-PTHKNINF-READ-SEC     SECTION.
      *
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key8"          TO  MCP-PATHNAME
               PERFORM 900-PTHKNINF-READ-N-SEC
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
          .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者保険マスター読込処理
      ******************************************************************
       900-PTHKNINF-READ-N-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTHKNINF-REC
      *         
               IF      PTHKN-HKNNUM (1:1)
                                       NOT =   "0"
                   GO  TO  900-PTHKNINF-READ-N-SEC
               END-IF
               IF  (   PTHKN-SAKJOKBN      =   "1"                     )
               OR  ( ( PTHKN-HKNNUM        =   "002"   OR  "031"   OR
                                               "032"   OR  "033"   OR
                                               "034"                  )
                 AND ( PTHKN-HOJOKBN       =   "A" OR  "B" OR  "C" OR
                                               "D" OR  "E" OR  "F" OR
                                               "G" OR  "H" OR  "I" OR
                                               "1" OR  "2" OR  "3"    ))
                   GO  TO  900-PTHKNINF-READ-N-SEC
               ELSE    
                   MOVE    ZERO                TO  FLG-PTHKNINF
               END-IF    
           ELSE
               INITIALIZE                      PTHKNINF-REC
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           .
       900-PTHKNINF-READ-N-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者マスター読込処理
      ******************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                      PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                      PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.

