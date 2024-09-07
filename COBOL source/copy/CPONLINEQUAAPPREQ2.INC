      *****************************************************************
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
      *****************************************************************
      *    ＡＰＩ資格確認用       COPY (CPONLINEQUAAPPREQ2.INC)          *
      *****************************************************************
       01  XML-ONLINEQUAAPPREQ2.
           03  QUAAPPREQ2-CONTEXT             PIC S9(9)   BINARY.
           03  QUAAPPREQ2-OBJECT              PIC X(36).
           03  QUAAPPREQ2-MODE                PIC S9(9)   BINARY.
           03  QUAAPPREQ2-RECORDNAME          PIC X(128).
           03  QUAAPPREQ2-DATAREQ.
      *メッセージヘッダー部     
               05  QUAAPPREQ2-HEADER.
      *処理実行日時
                   07  QUAAPPREQ2-SHORI-TIME  PIC X(14).
      *資格確認日
                   07  QUAAPPREQ2-KAKUNINYMD  PIC X(08).
      *医療機関コード
                   07  QUAAPPREQ2-HOSPCD      PIC X(10).
      *任意のファイル識別子(医療機関固有項目)
                   07  QUAAPPREQ2-FILE-SIKIBETU PIC X(50).
      *受付番号
                   07  QUAAPPREQ2-SHOKAI-KBN  PIC X(38).
      *処理結果区分
                   07  QUAAPPREQ2-RESULT-KBN  PIC X(01).
      *エラーコード
                   07  QUAAPPREQ2-ERR-CODE    PIC X(09).
      *エラーメッセージ
                   07  QUAAPPREQ2-ERR-MSG     PIC X(60).
      *総処理件数             
                   07  QUAAPPREQ2-ALL-CNT     PIC X(4).
      *処理件数(正常)             
                   07  QUAAPPREQ2-OK-CNT      PIC X(4).
      *処理件数(エラー)            
                   07  QUAAPPREQ2-ERR-CNT     PIC X(4).
      *文字コード識別
                   07  QUAAPPREQ2-CODE-TYPE   PIC X(1).
      *メッセージボディ部    
               05  QUAAPPREQ2-MESSAGEBODY     OCCURS  300.
      *個人単位情報
                   07  QUAAPPREQ2-KOJIN-AREA.
      *保険者番号
                       09  QUAAPPREQ2-SHO-HKNJANUM    PIC X(08).
      *記号
                       09  QUAAPPREQ2-SHO-KIGO        PIC X(80).
      *番号
                       09  QUAAPPREQ2-SHO-NUM         PIC X(80).
      *番号枝番
                       09  QUAAPPREQ2-SHO-EDABAN      PIC X(02).
      *生年月日
                       09  QUAAPPREQ2-SHO-BIRTHDAY    PIC X(08).
      *限度額適用認定証区分
                       09  QUAAPPREQ2-GENDO-SHOKBN    PIC X(02).
      *限度額適用認定証適用区分
                       09  QUAAPPREQ2-GENDO-TEKKBN    PIC X(03).
      *任意の識別子(医療機関固有項目)
                       09  QUAAPPREQ2-SHO-SIKIBETU    PIC X(50).

      *処理結果状況
                   07  QUAAPPREQ2-RESULT              PIC X(01).
      *処理結果コード
                   07  QUAAPPREQ2-RESULT-CODE         PIC X(09).
      *処理結果メッセージ
                   07  QUAAPPREQ2-RESULT-MSG          PIC X(60).
      *資格有効性
                   07  QUAAPPREQ2-SIKAKU-YUKO         PIC X(01).
      *確認結果リスト
                   07  QUAAPPREQ2-RESULT-AREA         OCCURS  1.
      *被保険者証区分(確認結果内容)
                       09  QUAAPPREQ2-CARD-CLASS          PIC X(02).
      *保険者番号
                       09  QUAAPPREQ2-RES-HKNJANUM        PIC X(08).
      *記号
                       09  QUAAPPREQ2-RES-KIGO            PIC X(80).
      *番号
                       09  QUAAPPREQ2-RES-NUM             PIC X(80).
      *番号枝番
                       09  QUAAPPREQ2-RES-EDABAN          PIC X(02).
      *本人家族区分
                       09  QUAAPPREQ2-RES-HONKZKKBN       PIC X(01).
      *被保険者氏名(世帯主氏名)
                       09  QUAAPPREQ2-RES-HIHKNJANAME     PIC X(100).
      *氏名
                       09  QUAAPPREQ2-RES-NAME            PIC X(100).
      *氏名（その他）
                       09  QUAAPPREQ2-RES-NAME-ETC        PIC X(100).
      *氏名カナ
                       09  QUAAPPREQ2-RES-KANANAME        PIC X(100).
      *氏名カナ（その他）
                       09  QUAAPPREQ2-RES-KANANAME-ETC    PIC X(100).
      *性別１
                       09  QUAAPPREQ2-RES-SEX1            PIC X(01).
      *性別２
                       09  QUAAPPREQ2-RES-SEX2            PIC X(01).
      *生年月日
                       09  QUAAPPREQ2-RES-BIRTHDAY        PIC X(08).
      *住所
                       09  QUAAPPREQ2-RES-ADRS            PIC X(250).
      *郵便番号
                       09  QUAAPPREQ2-RES-POST            PIC X(08).
      *
      *被保険者証交付年月日
                       09  QUAAPPREQ2-SKKGETYMD           PIC X(08).
      *被保険者証有効開始年月日
                       09  QUAAPPREQ2-TEKSTYMD            PIC X(08).
      *被保険者証有効終了年月日
                       09  QUAAPPREQ2-TEKEDYMD            PIC X(08).
      *被保険者証一部負担金割合
                       09  QUAAPPREQ2-FTNRATE             PIC X(03).
      *未就学区分
                       09  QUAAPPREQ2-MISYUGAKU           PIC X(01).
      *資格喪失事由
                       09  QUAAPPREQ2-LOSS-JIYU           PIC X(02).
      *保険者名称
                       09  QUAAPPREQ2-HKNJANUM-NAME       PIC X(200).
      *
      *高齢受給者証交付年月日
                       09  QUAAPPREQ2-ELDER-SKKGETYMD     PIC X(08).
      *高齢受給者証有効開始年月日
                       09  QUAAPPREQ2-ELDER-TEKSTYMD      PIC X(08).
      *高齢受給者証有効終了年月日
                       09  QUAAPPREQ2-ELDER-TEKEDYMD      PIC X(08).
      *高齢受給者一部負担金割合
                       09  QUAAPPREQ2-ELDER-FTNRATE       PIC X(03).
      *
      *限度額適用認定証提供同意フラグ
                       09  QUAAPPREQ2-GENDO-DOUIFLG       PIC X(01).
      *照会番号
                       09  QUAAPPREQ2-SHOKAI-NUM          PIC X(50).
