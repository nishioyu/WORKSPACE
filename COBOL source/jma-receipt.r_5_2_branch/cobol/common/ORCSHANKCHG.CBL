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
       PROGRAM-ID.             ORCSHANKCHG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 全角→半角変換処理
      *  返却エラーコード  : 0:正常　1:変換エラー
      *  管理者            : 
      *  作成日付    作業者        記述
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
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-ERR             PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-KANJI           PIC 9(01).
           03  FLG-KANJI-ERR       PIC 9(01).
           03  FLG-KUHAKU          PIC 9(01).
           03  FLG-ZENKAKU1        PIC 9(01).
           03  FLG-ZENKAKU2        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDX-MAX2            PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
      *
      *
        01  WRK-AREA.
           03  WRK-WYMD.
               05  WRK-WGO             PIC X(01).
               05  WRK-WYY             PIC 9(02).
               05  WRK-WMM             PIC 9(02).
               05  WRK-WDD             PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-INDAY.
               05  WRK-YY              PIC X(10).
               05  WRK-MM              PIC X(02).
               05  WRK-DD              PIC X(02).
      *
           03  WRK-HMM                 PIC 9(02).
           03  WRK-HDD                 PIC 9(02).
      *
           03  WRK-MOJI                PIC X(02).
      *
           03  WRK-KIGO                PIC X(01).
           03  WRK-ZENMOJI             PIC X(02).
           03  WRK-ZENMOJI2            PIC X(02).
      *
           03  WRK-HANKAKU-OT          PIC X(1000).
      *
           03  WRK-ERRCD               PIC X(02).
      *
           03  WRK-MAX-CNT             PIC 9(04).
      *
           03  WRK-IDX1                PIC 9(04).
           03  WRK-IDX2                PIC 9(04).
      *R01.8
           03  WRK-PCNT                PIC 9(04).
      *
      *    変換テーブル
       01  TBL-HENKAN-AREA.
           03  FILLER              PIC X(01)   VALUE   "0".
           03  FILLER              PIC X(02)   VALUE   "０".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(02)   VALUE   "１".
           03  FILLER              PIC X(01)   VALUE   "2".
           03  FILLER              PIC X(02)   VALUE   "２".
           03  FILLER              PIC X(01)   VALUE   "3".
           03  FILLER              PIC X(02)   VALUE   "３".
           03  FILLER              PIC X(01)   VALUE   "4".
           03  FILLER              PIC X(02)   VALUE   "４".
           03  FILLER              PIC X(01)   VALUE   "5".
           03  FILLER              PIC X(02)   VALUE   "５".
           03  FILLER              PIC X(01)   VALUE   "6".
           03  FILLER              PIC X(02)   VALUE   "６".
           03  FILLER              PIC X(01)   VALUE   "7".
           03  FILLER              PIC X(02)   VALUE   "７".
           03  FILLER              PIC X(01)   VALUE   "8".
           03  FILLER              PIC X(02)   VALUE   "８".
           03  FILLER              PIC X(01)   VALUE   "9".
           03  FILLER              PIC X(02)   VALUE   "９".
      *英字
           03  FILLER              PIC X(01)   VALUE   "A".
           03  FILLER              PIC X(02)   VALUE   "Ａ".
           03  FILLER              PIC X(01)   VALUE   "B".
           03  FILLER              PIC X(02)   VALUE   "Ｂ".
           03  FILLER              PIC X(01)   VALUE   "C".
           03  FILLER              PIC X(02)   VALUE   "Ｃ".
           03  FILLER              PIC X(01)   VALUE   "D".
           03  FILLER              PIC X(02)   VALUE   "Ｄ".
           03  FILLER              PIC X(01)   VALUE   "E".
           03  FILLER              PIC X(02)   VALUE   "Ｅ".
           03  FILLER              PIC X(01)   VALUE   "F".
           03  FILLER              PIC X(02)   VALUE   "Ｆ".
           03  FILLER              PIC X(01)   VALUE   "G".
           03  FILLER              PIC X(02)   VALUE   "Ｇ".
           03  FILLER              PIC X(01)   VALUE   "H".
           03  FILLER              PIC X(02)   VALUE   "Ｈ".
           03  FILLER              PIC X(01)   VALUE   "I".
           03  FILLER              PIC X(02)   VALUE   "Ｉ".
           03  FILLER              PIC X(01)   VALUE   "J".
           03  FILLER              PIC X(02)   VALUE   "Ｊ".
           03  FILLER              PIC X(01)   VALUE   "K".
           03  FILLER              PIC X(02)   VALUE   "Ｋ".
           03  FILLER              PIC X(01)   VALUE   "L".
           03  FILLER              PIC X(02)   VALUE   "Ｌ".
           03  FILLER              PIC X(01)   VALUE   "N".
           03  FILLER              PIC X(02)   VALUE   "Ｎ".
           03  FILLER              PIC X(01)   VALUE   "M".
           03  FILLER              PIC X(02)   VALUE   "Ｍ".
           03  FILLER              PIC X(01)   VALUE   "O".
           03  FILLER              PIC X(02)   VALUE   "Ｏ".
           03  FILLER              PIC X(01)   VALUE   "P".
           03  FILLER              PIC X(02)   VALUE   "Ｐ".
           03  FILLER              PIC X(01)   VALUE   "Q".
           03  FILLER              PIC X(02)   VALUE   "Ｑ".
           03  FILLER              PIC X(01)   VALUE   "R".
           03  FILLER              PIC X(02)   VALUE   "Ｒ".
           03  FILLER              PIC X(01)   VALUE   "S".
           03  FILLER              PIC X(02)   VALUE   "Ｓ".
           03  FILLER              PIC X(01)   VALUE   "T".
           03  FILLER              PIC X(02)   VALUE   "Ｔ".
           03  FILLER              PIC X(01)   VALUE   "U".
           03  FILLER              PIC X(02)   VALUE   "Ｕ".
           03  FILLER              PIC X(01)   VALUE   "V".
           03  FILLER              PIC X(02)   VALUE   "Ｖ".
           03  FILLER              PIC X(01)   VALUE   "W".
           03  FILLER              PIC X(02)   VALUE   "Ｗ".
           03  FILLER              PIC X(01)   VALUE   "X".
           03  FILLER              PIC X(02)   VALUE   "Ｘ".
           03  FILLER              PIC X(01)   VALUE   "Y".
           03  FILLER              PIC X(02)   VALUE   "Ｙ".
           03  FILLER              PIC X(01)   VALUE   "Z".
           03  FILLER              PIC X(02)   VALUE   "Ｚ".
      *英字
           03  FILLER              PIC X(01)   VALUE   "a".
           03  FILLER              PIC X(02)   VALUE   "ａ".
           03  FILLER              PIC X(01)   VALUE   "b".
           03  FILLER              PIC X(02)   VALUE   "ｂ".
           03  FILLER              PIC X(01)   VALUE   "c".
           03  FILLER              PIC X(02)   VALUE   "ｃ".
           03  FILLER              PIC X(01)   VALUE   "d".
           03  FILLER              PIC X(02)   VALUE   "ｄ".
           03  FILLER              PIC X(01)   VALUE   "e".
           03  FILLER              PIC X(02)   VALUE   "ｅ".
           03  FILLER              PIC X(01)   VALUE   "f".
           03  FILLER              PIC X(02)   VALUE   "ｆ".
           03  FILLER              PIC X(01)   VALUE   "g".
           03  FILLER              PIC X(02)   VALUE   "ｇ".
           03  FILLER              PIC X(01)   VALUE   "h".
           03  FILLER              PIC X(02)   VALUE   "ｈ".
           03  FILLER              PIC X(01)   VALUE   "i".
           03  FILLER              PIC X(02)   VALUE   "ｉ".
           03  FILLER              PIC X(01)   VALUE   "j".
           03  FILLER              PIC X(02)   VALUE   "ｊ".
           03  FILLER              PIC X(01)   VALUE   "k".
           03  FILLER              PIC X(02)   VALUE   "ｋ".
           03  FILLER              PIC X(01)   VALUE   "l".
           03  FILLER              PIC X(02)   VALUE   "ｌ".
           03  FILLER              PIC X(01)   VALUE   "n".
           03  FILLER              PIC X(02)   VALUE   "ｎ".
           03  FILLER              PIC X(01)   VALUE   "m".
           03  FILLER              PIC X(02)   VALUE   "ｍ".
           03  FILLER              PIC X(01)   VALUE   "o".
           03  FILLER              PIC X(02)   VALUE   "ｏ".
           03  FILLER              PIC X(01)   VALUE   "p".
           03  FILLER              PIC X(02)   VALUE   "ｐ".
           03  FILLER              PIC X(01)   VALUE   "q".
           03  FILLER              PIC X(02)   VALUE   "ｑ".
           03  FILLER              PIC X(01)   VALUE   "r".
           03  FILLER              PIC X(02)   VALUE   "ｒ".
           03  FILLER              PIC X(01)   VALUE   "s".
           03  FILLER              PIC X(02)   VALUE   "ｓ".
           03  FILLER              PIC X(01)   VALUE   "t".
           03  FILLER              PIC X(02)   VALUE   "ｔ".
           03  FILLER              PIC X(01)   VALUE   "u".
           03  FILLER              PIC X(02)   VALUE   "ｕ".
           03  FILLER              PIC X(01)   VALUE   "v".
           03  FILLER              PIC X(02)   VALUE   "ｖ".
           03  FILLER              PIC X(01)   VALUE   "w".
           03  FILLER              PIC X(02)   VALUE   "ｗ".
           03  FILLER              PIC X(01)   VALUE   "x".
           03  FILLER              PIC X(02)   VALUE   "ｘ".
           03  FILLER              PIC X(01)   VALUE   "y".
           03  FILLER              PIC X(02)   VALUE   "ｙ".
           03  FILLER              PIC X(01)   VALUE   "z".
           03  FILLER              PIC X(02)   VALUE   "ｚ".
      *
           03  FILLER              PIC X(01)   VALUE   "-".
           03  FILLER              PIC X(02)   VALUE   "−".
           03  FILLER              PIC X(01)   VALUE   "(".
           03  FILLER              PIC X(02)   VALUE   "（".
           03  FILLER              PIC X(01)   VALUE   ")".
           03  FILLER              PIC X(02)   VALUE   "）".
           03  FILLER              PIC X(01)   VALUE   "\".
           03  FILLER              PIC X(02)   VALUE   "￥".
           03  FILLER              PIC X(01)   VALUE   ".".
           03  FILLER              PIC X(02)   VALUE   "．".
           03  FILLER              PIC X(01)   VALUE   "/".
           03  FILLER              PIC X(02)   VALUE   "／".
           03  FILLER              PIC X(01)   VALUE   "<".
           03  FILLER              PIC X(02)   VALUE   "＜".
           03  FILLER              PIC X(01)   VALUE   ">".
           03  FILLER              PIC X(02)   VALUE   "＞".
           03  FILLER              PIC X(01)   VALUE   "*".
           03  FILLER              PIC X(02)   VALUE   "＊".
           03  FILLER              PIC X(01)   VALUE   "?".
           03  FILLER              PIC X(02)   VALUE   "？".
           03  FILLER              PIC X(01)   VALUE   "%".
           03  FILLER              PIC X(02)   VALUE   "％".
           03  FILLER              PIC X(01)   VALUE   "+".
           03  FILLER              PIC X(02)   VALUE   "＋".
           03  FILLER              PIC X(01)   VALUE   "!".
           03  FILLER              PIC X(02)   VALUE   "！".
           03  FILLER              PIC X(01)   VALUE   "#".
           03  FILLER              PIC X(02)   VALUE   "＃".
           03  FILLER              PIC X(01)   VALUE   "&".
           03  FILLER              PIC X(02)   VALUE   "＆".
           03  FILLER              PIC X(01)   VALUE   "=".
           03  FILLER              PIC X(02)   VALUE   "＝".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(02)   VALUE   "，".
           03  FILLER              PIC X(01)   VALUE   "_".
           03  FILLER              PIC X(02)   VALUE   "＿".
           03  FILLER              PIC X(01)   VALUE   "@".
           03  FILLER              PIC X(02)   VALUE   "＠".
           03  FILLER              PIC X(01)   VALUE   "[".
           03  FILLER              PIC X(02)   VALUE   "［".
           03  FILLER              PIC X(01)   VALUE   "]".
           03  FILLER              PIC X(02)   VALUE   "］".
      *
       01  TBL-HENKAN-AREA-R           REDEFINES   TBL-HENKAN-AREA.
           03  TBL-HENKAN-OCC          OCCURS   83
                                       INDEXED  TBL-IDX.
               05  TBL-HALF            PIC X(01).
               05  TBL-ZEN             PIC X(02).
       01  TBL-MAX                     PIC 9(04)   VALUE   83.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCXKANACONV2.INC".
      *
      *    拡張文字対応
           COPY    "CPORCSSTRING.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSHANKCHG.INC".
      *
       PROCEDURE                    DIVISION    USING
           ORCSHANKCHGAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    ZERO                TO  HANKCHG-RC
           MOVE    ZERO                TO  HANKCHG-RC2
           MOVE    SPACE               TO  HANKCHG-OUT-INPUT
      *    最大桁数判定
           MOVE    ZERO                TO  IDX-MAX
           PERFORM VARYING     IDX     FROM    1000 BY  -1
                   UNTIL       IDX     <   1
               IF     HANKCHG-MAE-INPUT (IDX:1)    NOT =   SPACE
                   MOVE    IDX             TO  IDX-MAX
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
           IF      IDX-MAX             =   ZERO
               MOVE    ZERO                TO  HANKCHG-MAX
               MOVE    SPACE               TO  HANKCHG-OUT-INPUT
           ELSE
               MOVE    IDX-MAX             TO  HANKCHG-MAX
           END-IF
      *TEST
      *    DISPLAY " HANKCHG-MAX:"  HANKCHG-MAX
      *    DISPLAY " HANKCHG-MAE-INPUT:"  HANKCHG-MAE-INPUT
      *                    (1:HANKCHG-MAX)
      *
           IF      IDX-MAX             >   ZERO
      *        全角の内容判定
               PERFORM 1000-KOUMOKU-CHK-SEC
      *
      *        全角判定処理
               PERFORM 100-ZENKAKU-CHECK-SEC
      *        半角のみは、変換なし
               IF     (FLG-KANA            =   1    )
                 OR   (HANKCHG-RC      NOT =   ZERO )
                   CONTINUE
               ELSE
      *            全角置換処理
                   PERFORM 200-ZENKAKU-HENKAN-SEC
      *            半角変換（英数、全角カナのみ）
                   IF      HANKCHG-RC      =   ZERO
                       PERFORM 300-HANKAKU-HENKAN-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    全角チェック処理
      *****************************************************************
       1000-KOUMOKU-CHK-SEC           SECTION.
      *
      *    半角カナ、外字（■）変換
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    IDX-MAX             TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    HANKCHG-MAE-INPUT   TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
      *
           MOVE    KANACONV-OUTDATA    TO  HANKCHG-MAE-INPUT
           MOVE    KANACONV-OUTDATA    TO  HANKCHG-OUT-INPUT
           IF      KANACONV-MAXLEN     =   IDX-MAX
               MOVE    ZERO                TO  HANKCHG-RC2
           ELSE
               MOVE    7                   TO  HANKCHG-RC2
           END-IF
           MOVE    KANACONV-MAXLEN     TO  IDX-MAX
           MOVE    KANACONV-MAXLEN     TO  HANKCHG-MAX
           .
       1000-KOUMOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    改行判定処理
      *****************************************************************
       1009-KAIGYO-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-PCNT
           INSPECT     HANKCHG-OUT-INPUT  TALLYING  WRK-PCNT
                                      FOR ALL  X"0A"
      *    改行コードありは変換不可
           IF      WRK-PCNT            >   ZERO
               MOVE    2                   TO  HANKCHG-RC
           END-IF
      *
           .
       1009-KAIGYO-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    全角判定処理
      *****************************************************************
       100-ZENKAKU-CHECK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-KANJI
           MOVE    ZERO                TO  FLG-KANA
           MOVE    ZERO                TO  FLG-KUHAKU
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    HANKCHG-MAX         TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    15                  TO  KANACONV-CHAR-TYPE
           MOVE    HANKCHG-MAE-INPUT   TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           IF      KANACONV-RETURN     =   ZERO
      *        全角のみ
               MOVE    ZERO                TO  FLG-KANA
           ELSE
      *        半角のみの判定
               INITIALIZE                  ORCXKANACONVAREA
               MOVE    HANKCHG-MAX         TO  KANACONV-LEN
               MOVE    5000                TO  KANACONV-STLEN
               MOVE    0                   TO  KANACONV-CONV-FLAG
               MOVE    113                 TO  KANACONV-CHAR-TYPE
               MOVE    HANKCHG-MAE-INPUT   TO  KANACONV-INDATA
               CALL    "kanaconv"          USING
                                           ORCXKANACONVAREA
               IF      KANACONV-RETURN     =   ZERO
                   MOVE    1                   TO  FLG-KANA
                   MOVE    HANKCHG-MAE-INPUT   TO  HANKCHG-OUT-INPUT
                   MOVE    KANACONV-MAXLEN     TO  HANKCHG-MAX
               ELSE
      *            混在エラー
                   MOVE    2                   TO  FLG-KANA
               END-IF
           END-IF
      *test
           DISPLAY "CHG FLG-KANA:"  FLG-KANA
      *
      *    改行コードありは変換不可
           PERFORM 1009-KAIGYO-CHK-SEC
           .
       100-ZENKAKU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角変換定処理
      *****************************************************************
       300-HANKAKU-HENKAN-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-HANKAKU-OT
           MOVE    1                   TO  IDH2
           PERFORM VARYING     IDH1    FROM    1   BY  2
                   UNTIL      (IDH1        >   HANKCHG-MAX)
                        OR    (IDH2        >   1000 )
                        OR    (HANKCHG-RC  NOT =   ZERO )
               MOVE    HANKCHG-OUT-INPUT (IDH1:2)
                                           TO  WRK-MOJI
      *        英数値テーブル検索
               SET      TBL-IDX            TO  1
               SEARCH   TBL-HENKAN-OCC     VARYING   TBL-IDX
                   AT  END
                       MOVE    ZERO             TO  FLG-ARI
                   WHEN    WRK-MOJI             =   TBL-ZEN
                                                          (TBL-IDX)
                       MOVE    TBL-HALF  (TBL-IDX)
                                                TO  WRK-HANKAKU-OT
                                                            (IDH2:1)
                       ADD     1                TO  IDH2
                       MOVE    1                TO  FLG-ARI
               END-SEARCH
               IF      FLG-ARI             =   ZERO
                   EVALUATE   WRK-MOJI
      *                A8 A1
                       WHEN    "─"
      *                A1 DD
                       WHEN    "−"
      *                A1 BE
                       WHEN    "‐"
      *                A1 BD
                       WHEN    "―"
      *                A1 BC
                       WHEN    "ー"
                           MOVE    "-"          TO  WRK-HANKAKU-OT
                                                            (IDH2:1)
                           ADD     1            TO  IDH2
                           MOVE    1            TO  FLG-ARI 
      *               空白
                       WHEN    "　"
                           MOVE    SPACE        TO  WRK-HANKAKU-OT
                                                            (IDH2:1)
                           ADD     1            TO  IDH2
                           MOVE    1            TO  FLG-ARI
                   END-EVALUATE
               END-IF
      *
               IF      FLG-ARI             =   ZERO
      *            数値・英字対象なし
                   IF      HANKCHG-SYORI       =   "1"
      *                英数値のみ変換
                       MOVE    2               TO  HANKCHG-RC2
                       IF      HANKCHG-SYORIKBN     =   "1"
                           MOVE    "?"             TO  WRK-HANKAKU-OT
                                                            (IDH2:1)
                           ADD     1               TO  IDH2
                       END-IF
                   ELSE
      *                カナ文字変換
                       PERFORM 3001-KANAHENNKAN-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      HANKCHG-RC2         =   2
      *       変換不可ありエラー
              IF      HANKCHG-SYORIKBN     =   "2"
                  MOVE    1                TO  HANKCHG-RC
                  MOVE    HANKCHG-MAE-INPUT
                                           TO  HANKCHG-OUT-INPUT
              END-IF
           END-IF
           IF      HANKCHG-RC          =   ZERO
               MOVE    WRK-HANKAKU-OT      TO  HANKCHG-OUT-INPUT
               COMPUTE HANKCHG-MAX         =   IDH2    -   1
           END-IF
           .
       300-HANKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角カナ文字を半角カナへ変換処理
      *****************************************************************
       3001-KANAHENNKAN-SEC           SECTION.
      *
           EVALUATE    WRK-MOJI
                   WHEN    "ァ"
                       MOVE    "ｧ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ア"
                       MOVE    "ｱ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ィ"
                       MOVE    "ｨ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "イ"
                       MOVE    "ｲ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ゥ"
                       MOVE    "ｩ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ウ"
                       MOVE    "ｳ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ェ"
                       MOVE    "ｪ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "エ"
                       MOVE    "ｴ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ォ"
                       MOVE    "ｫ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "オ"
                       MOVE    "ｵ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
      *
                   WHEN    "カ"
                       MOVE    "ｶ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ガ"
                       MOVE    "ｶﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "キ"
                       MOVE    "ｷ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ギ"
                       MOVE    "ｷﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "ク"
                       MOVE    "ｸ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "グ"
                       MOVE    "ｸﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "ケ"
                       MOVE    "ｹ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ゲ"
                       MOVE    "ｹﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "コ"
                       MOVE    "ｺ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ゴ"
                       MOVE    "ｺﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
      *
                   WHEN    "サ"
                       MOVE    "ｻ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ザ"
                       MOVE    "ｻﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "シ"
                       MOVE    "ｼ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ジ"
                       MOVE    "ｼﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "ス"
                       MOVE    "ｽ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ズ"
                       MOVE    "ｽﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "セ"
                       MOVE    "ｾ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ゼ"
                       MOVE    "ｾﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "ソ"
                       MOVE    "ｿ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ゾ"
                       MOVE    "ｿﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
      *
                   WHEN    "タ"
                       MOVE    "ﾀ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ダ"
                       MOVE    "ﾀﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "チ"
                       MOVE    "ﾁ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ヂ"
                       MOVE    "ﾁﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "ッ"
                       MOVE    "ｯ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ツ"
                       MOVE    "ﾂ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ヅ"
                        MOVE  "ﾂﾞ"      TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "テ"
                       MOVE    "ﾃ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "デ"
                       MOVE  "ﾃﾞ"       TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "ト"
                       MOVE    "ﾄ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ド"
                       MOVE    "ﾄﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
      * 
                   WHEN    "ナ"
                       MOVE    "ﾅ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ニ"
                       MOVE    "ﾆ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                  WHEN    "ヌ"
                       MOVE    "ﾇ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ネ"
                       MOVE    "ﾈ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ノ"
                       MOVE    "ﾉ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
      *
                   WHEN    "ハ"
                       MOVE    "ﾊ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "バ"
                       MOVE    "ﾊﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "パ"
                       MOVE    "ﾊﾟ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "ヒ"
                       MOVE    "ﾋ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ビ"
                       MOVE    "ﾋﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "ピ"
                       MOVE    "ﾋﾟ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "フ"
                       MOVE    "ﾌ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ブ"
                       MOVE    "ﾌﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "プ"
                       MOVE    "ﾌﾟ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "ヘ"
                       MOVE    "ﾍ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ベ"
                       MOVE    "ﾍﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "ペ"
                       MOVE    "ﾍﾟ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "ホ"
                       MOVE    "ﾎ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ボ"
                       MOVE    "ﾎﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "ポ"
                       MOVE    "ﾎﾟ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
      *
                   WHEN    "マ"
                       MOVE    "ﾏ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ミ"
                       MOVE    "ﾐ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ム"
                       MOVE    "ﾑ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "メ"
                       MOVE    "ﾒ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "モ"
                       MOVE    "ﾓ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
      *
                   WHEN    "ャ"
                       MOVE    "ｬ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ヤ"
                       MOVE    "ﾔ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ュ"
                       MOVE    "ｭ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ユ"
                       MOVE    "ﾕ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ョ"
                       MOVE    "ｮ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ヨ"
                       MOVE    "ﾖ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
      *
                   WHEN    "ラ"
                       MOVE    "ﾗ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "リ"
                       MOVE    "ﾘ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ル"
                       MOVE    "ﾙ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "レ"
                       MOVE    "ﾚ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ロ"
                       MOVE    "ﾛ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
      *
                   WHEN    "ヮ"
                       MOVE    "ﾜ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ワ"
                       MOVE    "ﾜ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ヰ"
                       MOVE    "ｲ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ヱ"
                       MOVE    "ｴ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ヲ"
                       MOVE    "ｦ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ン"
                       MOVE    "ﾝ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ヴ"
                       MOVE    "ｳﾞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "ヵ"
                       MOVE    "ｶ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "ヶ"
                       MOVE    "ｹ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
      *
               WHEN    OTHER
      *            変換エラー
                   MOVE    2               TO  HANKCHG-RC2
                   IF      HANKCHG-SYORIKBN    =   "1"
                       MOVE    "?"             TO  WRK-HANKAKU-OT
                                                            (IDH2:1)
                       ADD     1               TO  IDH2
                  END-IF
           END-EVALUATE
      *
           .
       3001-KANAHENNKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角処理
      *****************************************************************
       200-ZENKAKU-HENKAN-SEC           SECTION.
      *
      **   MOVE    2                   TO  HANKCHG-SYUBETU
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    HANKCHG-MAX         TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    1                   TO  KANACONV-CONV-FLAG
           MOVE    15                  TO  KANACONV-CHAR-TYPE
           MOVE    HANKCHG-MAE-INPUT   TO  KANACONV-INDATA
           CALL    "kanaconv"          USING
                                       ORCXKANACONVAREA
           IF      KANACONV-RETURN     =   ZERO
               MOVE    KANACONV-OUTDATA    TO  HANKCHG-OUT-INPUT
               MOVE    KANACONV-MAXLEN     TO  HANKCHG-MAX
           ELSE
               MOVE    2                   TO  HANKCHG-RC
           END-IF
      *
           .
       200-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
