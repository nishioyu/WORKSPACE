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
      *    ＡＰＩ資格確認用       COPY (CPONLINEQUARES1.INC)          *
      *****************************************************************
       01  XML-ONLINEQUARES1.
           02  QUARES1-CONTEXT         PIC S9(9)   BINARY.
           02  QUARES1-OBJECT          PIC X(36).
           02  QUARES1-MODE            PIC S9(9)   BINARY.
           02  QUARES1-RECORDNAME      PIC X(128).
           02  QUARES1-QUARES1.
      *        実施日
               03  QUARES1-INFORMATION-DATE  PIC X(10).
      *        実施時間
               03  QUARES1-INFORMATION-TIME  PIC X(8).
      *        返却コード
               03  QUARES1-API-RESULT        PIC X(3).
      *        返却メッセージ
               03  QUARES1-API-RESULT-MSG    PIC X(100).
      *患者番号
               03  QUARES1-PATIENT-ID  PIC X(20).
      *資格確認要否(Qua)
               03  QUARES1-QUA-CLASS   PIC X(5).
      *照会番号登録要否(Ref)
               03  QUARES1-REF-CLASS   PIC X(5).
      *薬剤情報要否(Drg)
               03  QUARES1-DRG-CLASS   PIC X(5).
      *特定検診情報要否(Spec)
               03  QUARES1-SPEC-CLASS  PIC X(5).
      *資格確認
               03  QUARES1-MESSAGEHEADER-QUA.
      *資格確認日
                   04  QUARES1-KAKUNINYMD     PIC X(8).
      *医療機関コード          
                   04  QUARES1-HOSPCD         PIC X(10).
      *任意のファイル識別子(医療機関固有項目)
                   04  QUARES1-SIKIBETU       PIC X(50).
      *メッセージボディ部          
                   04  QUARES1-MESSAGEBODY-QUA.
      *資格確認照会用情報
                       05  QUARES1-QUA-INFO.
      *保険者番号
                           06  QUARES1-QUA-HKNJANUM    PIC X(08).
      *記号
                           06  QUARES1-QUA-KIGO        PIC X(80).
      *番号
                           06  QUARES1-QUA-NUM         PIC X(80).
      *番号枝番
                           06  QUARES1-QUA-EDABAN      PIC X(02).
      *生年月日
                           06  QUARES1-QUA-BIRTHDAY    PIC X(08).
      *限度額適用認定証提供同意フラグ
                           06  QUARES1-QUA-DOUIFLG     PIC X(01).
      *任意の識別子(医療機関固有項目)
                           06  QUARES1-QUA-SIKIBETU    PIC X(50).
      *処方箋発行形態
                           06  QUARES1-QUA-SHOHO-KEITAI
                                                       PIC X(01).
      *照会番号登録(Ref)
               03  QUARES1-MESSAGEHEADER-REF.
      *医療機関コード    
                   04  QUARES1-REF-HOSPCD      PIC X(10).
      *任意のファイル識別子(医療機関固有項目)
                   04  QUARES1-REF-SIKIBETU    PIC X(50).
      *メッセージボディ部
                   04  QUARES1-MESSAGEBODY-REF.
      *照会番号登録情報
                       05  QUARES1-REF-INFO.
      *照会番号
                           06  QUARES1-REF-SHOKAI-NUM  PIC X(50).
      *保険者番号
                           06  QUARES1-REF-HKNJANUM    PIC X(08).
      *記号
                           06  QUARES1-REF-KIGO        PIC X(80).
      *番号
                           06  QUARES1-REF-NUM         PIC X(80).
      *番号枝番
                           06  QUARES1-REF-EDABAN      PIC X(02).
      *薬剤情報(Drg)
               03  QUARES1-MESSAGEHEADER-DRG   OCCURS  13.
      *医療機関コード
                   04  QUARES1-DRG-HOSPCD      PIC X(10).
      *保険者番号
                   04  QUARES1-DRG-HKNJANUM    PIC X(08).
      *記号
                   04  QUARES1-DRG-KIGO        PIC X(80).
      *番号
                   04  QUARES1-DRG-NUM         PIC X(80).
      *番号枝番
                   04  QUARES1-DRG-EDABAN      PIC X(02).
      *任意のファイル識別子(医療機関固有項目)
                   04  QUARES1-DRG-SIKIBETU    PIC X(50).
      *メッセージボディ部
                   04  QUARES1-MESSAGEBODY-DRG.
      *開始年月(202109 以降とすること)
                       05  QUARES1-DRG-STARTDATE   PIC X(6).
      *終了年月(999999 は、当月扱い。最長36ヶ月(3年)とすること)
                       05  QUARES1-DRG-ENDDATE     PIC X(6).
      *前回取得日
                       05  QUARES1-DRG-LASTDATE    PIC X(8).
      *要求ファイル種別
                       05  QUARES1-DRG-FILECATEGORY
                                                   PIC X(1).
      *要求ファイル
                       05  QUARES1-DRG-FILE        PIC X(38).
               03  QUARES1-MESSAGEHEADER-SPEC  OCCURS  2.
      *医療機関コード
                   04  QUARES1-SPEC-HOSPCD     PIC X(10).
      *保険者番号
                   04  QUARES1-SPEC-HKNJANUM   PIC X(08).
      *記号
                   04  QUARES1-SPEC-KIGO       PIC X(80).
      *番号
                   04  QUARES1-SPEC-NUM        PIC X(80).
      *番号枝番
                   04  QUARES1-SPEC-EDABAN     PIC X(02).
      *任意のファイル識別子(医療機関固有項目)
                   04  QUARES1-SPEC-SIKIBETU   PIC X(50).
      *メッセージボディ部
                   04  QUARES1-MESSAGEBODY-SPEC.
      *要求ファイル種別
                       05  QUARES1-SPEC-FILECATEGORY
                                               PIC X(1).
      *要求ファイル
                       05  QUARES1-SPEC-FILE   PIC X(38).
