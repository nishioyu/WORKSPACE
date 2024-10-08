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
      ************************************************************
      *    レセプト明細書     COPY (CPRCF065.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03.01.17  データ区分の追加
      *  01.00.02    NACL-門脇    03.09.29  包括対象検査情報の追加
      *                                     検査等実施判断情報の追加
      *                                     各種番号等情報の追加
      *  01.00.03    NACL-門脇    04.06.07  繰り返しを集団項目へ変更
      *
      *  01.00.04    NACL-藤原    04/07/27  診療種別区分
      *                                     告示等識別区分１
      *                                     きざみ値計算識別
      *                                     注加算コードの追加
      *  01.00.05    NACL-門脇    05/01/17  その他情報の追加
      *
      *  02.06.01    NACL-藤原    05/08/22  検体検査コメントの追加
      *
      *  02.09.01    NACL-藤原    06/04/13  検査等実施判断グループの追加
      *  03.04.01    NACL-門脇    07/02/19  レセプト対象外フラグの追加
      *  03.04.02    NACL-藤原    07/03/27  特定器材種別１、酸素等区分の追加
      *  03.05.01    NACL-小豆沢  07/05/30  グループ診療対応
      *  04.01.01    NACL-門脇    07/11/26  特定器材商品名記載対応
      *
      *  04.04.01    NACL-藤原    09/04/21  告示等識別区分２、通則加算対
      *                                     象外区分の追加
      *  04.04.02    NACL-藤原    09/12/07  時間外加算区分の追加
      *
      *  04.05.01    NACL-藤原    09/07/14  小数点以下５桁対応
      *  04.05.02    NACL-門脇    09/11/30  レセ電フォーマットコメント拡張
      *                                     対応（ファイル編集）
      *
      *  04.06.01    NACL-藤原    10/06/23  レセ電請求時の受付エラー分の
      *                                     種別不明レセ対応
      *  04.06.02    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *
      *  04.07.01    NACL-藤原    12/12/17  複数枚レセプト分割対応
      *
      *  04.08.01    NACL-門脇    16/01/12  点検用レセプト院外処方
      *                                     一般名記載対応
      *
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      *  05.00.02    NACL-門脇    20/05/19  令和２年４月改正対応
      *  05.02.01    ORCAMO       24/04/26  令和６年度改正対応
      *                                     CDKBN-KOUBANを２桁から３桁に変更
      ************************************************************
      *01  RECE65-REC.
           03  RECE65-KEY.
               05  RECE65-KEY1.
      *医療機関ＩＤ
                   07  RECE65-HOSPNUM      PIC 9(2).
      *診療年月
                   07  RECE65-SRYYM        PIC 9(06).
      *入外区分
                   07  RECE65-NYUGAIKBN    PIC X(01).
      *患者ＩＤ
                   07  RECE65-PTID         PIC 9(10).
      *レセプト科
                   07  RECE65-RECEKA       PIC X(02).
      *審査支払機関
                   07  RECE65-TEISYUTUSAKI
                                           PIC 9(01).
      *レセプト種別
                   07  RECE65-RECESYUBETU  PIC 9(04).
      *保険者番号        
                   07  RECE65-HKNJANUM     PIC X(08).
      *補助区分
                   07  RECE65-HOJOKBN      PIC X(01).
      *公費ＩＤ
                   07  RECE65-KOHID        PIC 9(10).
      *適用開始年月日
                   07  RECE65-TEKSTYMD     PIC X(08). 
      *分割区分
                   07  RECE65-RECEIPT-KBN  PIC X(10).
      *剤番号
               05  RECE65-ZAINUM          PIC  9(08).
      *剤連番
               05  RECE65-ZAIREN          PIC  9(02).
      *連番号
               05  RECE65-RENNUM          PIC  9(02).
      *公費分離区分
           03  RECE65-KOHBUNRI            PIC  9(01).
      *患者番号
           03  RECE65-PTNUM               PIC  X(20).
      *診療科
           03  RECE65-SRYKA               PIC  X(02).
      *診療区分
           03  RECE65-SRYKBN              PIC  X(02).
      *診療種別区分
           03  RECE65-SRYSYUKBN           PIC  X(03).
      *自費金額
           03  RECE65-JIHIMONEY           PIC  9(07).
      *
           03  RECE65-SRYACT-TBL          OCCURS  5.
      *診療行為情報
               05  RECE65-SRYACTINF.
      *診療コード
                   07  RECE65-SRYCD           PIC  X(09).
      *診療種別区分
                   07  RECE65-SRYSYUKBN-X     PIC  X(03).
      *数量
                   07  RECE65-SRYSURYO        PIC  9(05)V9(05).
      *回数
                   07  RECE65-SRYKAISU        PIC  9(03).
      *単位コード
                   07  RECE65-TANICD          PIC  9(03).
      *単位名称有効桁数
                   07  RECE65-TANIMOJISU      PIC  9(01).
      *単位名称
                   07  RECE65-TANINAME        PIC  X(24).
      *点数識別区分
                   07  RECE65-TENSIKIBETU     PIC  9(01).
      *点数
                   07  RECE65-TEN             PIC  9(09)V9(02).
      *明細請求フラグ
                   07  RECE65-MEISKYFLG       PIC  X(01).
      *自動算定区分
                   07  RECE65-AUTOKBN         PIC  X(01).
      *名称入力番号
                   07  RECE65-INPUTNUM        PIC  9(03).
      *点数欄集計先識別
                   07  RECE65-TENTTLKBN       PIC  9(03).
      *データ区分
                   07  RECE65-DATAKBN         PIC  9(01).
      *包括対象検査
                   07  RECE65-HOUKNSKBN       PIC  9(02).
      *検査等実施判断
                   07  RECE65-KNSJISKBN       PIC  9(01).
      *検査等実施判断グループ
                   07  RECE65-KNSJISGRPKBN    PIC  9(02).
      *各種番号等
                   07  RECE65-CDKBNINF.
                       09  RECE65-CDKBN-KBN        PIC  X(01).
                       09  RECE65-CDKBN-SYO        PIC  9(01).
                       09  RECE65-CDKBN-BU         PIC  9(02).
                       09  RECE65-CDKBN-KBNNUM     PIC  9(03).
                       09  RECE65-CDKBN-KBNNUM-EDA PIC  9(02).
                       09  RECE65-CDKBN-KOUBAN     PIC  9(03).
                       09  RECE65-KOUHYOJYUNNUM    PIC  9(09).
      *告示等識別区分１
                   07  RECE65-KOKUJISKBKBN1   PIC  9(01).
      *告示等識別区分２
                   07  RECE65-KOKUJISKBKBN2   PIC  9(01).
      *通則加算対象外区分
                   07  RECE65-TUSOKUGAIKBN    PIC  9(01).
      *時間外加算区分
                   07  RECE65-TIMEKSNKBN      PIC  9(01).
      *きざみ値計算識別
                   07  RECE65-KZMCOMPSIKIBETU PIC  9(01).
      *注加算コード
                   07  RECE65-CHUKSNINF.
      *コード
                       09  RECE65-CHUKSNCD     PIC  X(04).
      *通番
                       09  RECE65-CHUKSNTUBAN  PIC  X(01).
      *検体検査コメント
                   07  RECE65-KENTAICOMMENT    PIC  9(01).
      *入力コメント
                   07  RECE65-INPUTCOMENT      PIC  X(140).
      *名称情報
               05  RECE65-NAMEINF.
      *漢字有効桁数
                   07  RECE65-YUKOKETA         PIC  9(03).
      *漢字名称
                   07  RECE65-NAME             PIC  X(300).
      *その他情報
               05  RECE65-SONOTAINF.
      *特定器材種別１
                   07  RECE65-TOKUKIZAISBT1    PIC  9(01).
      *酸素等区分
                   07  RECE65-SANSOKBN         PIC  9(01).
      *レセプト編集情報
                   07  RECE65-SSTKIJUNCDTBL    OCCURS 10.
                       09  RECE65-SSTKIJUNCD   PIC  9(03).
      *
      *レセプト電算対象外フラグ
                   07  RECE65-RECEDENFLG       PIC  X(01).
      *レセプト対象外フラグ
                   07  RECE65-RECEPTFLG        PIC  X(01).
      *レセ電用特定器材商品名
                   07  RECE65-RECEDENNAME      PIC  X(300).
      *レセ用特定器材商品名編集フラグ
                   07  RECE65-RECEHENFLG       PIC  X(01).
      *関係コメント関連設定フラグ
                   07  RECE65-SETCOMATFLG      PIC  9(03).
      *コメントコード置き換えフラグ
                   07  RECE65-COMCDCHGFLG      PIC  X(01).
      *
      *一般名記載院外処方フラグ
                   07  RECE65-INGAIFLG         PIC  X(01).
      *一般名記載院外処方フラグ（レセプト用）
                   07  RECE65-INGAIFLG-RECE    PIC  X(01).
      *一般名コード（薬価基準コード）
                   07  RECE65-YAKKAKJNCD       PIC  X(12).
      *一般名処方加算区分
                   07  RECE65-GENERIC-KBN      PIC  X(01).
      *一般名称
                   07  RECE65-GENERICNAME      PIC  X(200).
      *漢字有効桁数
                   07  RECE65-GENERICYUKOKETA  PIC  9(03).
      *
