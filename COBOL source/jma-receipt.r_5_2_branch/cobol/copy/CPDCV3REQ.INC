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
      ******************************************************************
      *    HAORIリクエストデータチェック情報
      *    COPY (DCV3REQ.INC)
      ******************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.01.01    ORCAMO       22/10/26  枝番データチェック対応
      ************************************************************
       01  DCREQ-AREA.
           03  DCREQ-CONTEXT                   PIC S9(9)   BINARY.
           03  DCREQ-OBJECT                    PIC X(36).
           03  DCREQ-MODE                      PIC S9(9)   BINARY.
           03  DCREQ-RECORDNAME                PIC X(128).
           03  DCREQ-DATA-AREA.
               05  DCREQ-REQNUM                PIC X(2).
               05  DCREQ-KARTE-UID             PIC X(36).
               05  DCREQ-ORCA-UID              PIC X(36).
               05  DCREQ-SRYYM                 PIC X(07).
               05  DCREQ-STDAY                 PIC X(2).
               05  DCREQ-EDDAY                 PIC X(2).
               05  DCREQ-NYUGAIKBN             PIC X(1).
               05  DCREQ-HOKEN-AREA.
                   07  DCREQ-HOKEN-OCC         OCCURS 6.
                       09  DCREQ-HOKENID       PIC X(50).
               05  DCREQ-ITEM-AREA.
                   07  DCREQ-ITEM-OCC          OCCURS 22.
                       09  DCREQ-ITEMID        PIC X(50).
               05  DCREQ-INGAIPROCKBN          PIC X(1).
               05  DCREQ-PRTSRTKBN             PIC X(1).
               05  DCREQ-PTINF-AREA.
                   07  DCREQ-PTINF-OCC         OCCURS  100.
                       09  DCREQ-PTNUM         PIC X(20).
