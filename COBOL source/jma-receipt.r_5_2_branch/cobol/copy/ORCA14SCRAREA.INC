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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      *****************************************************************
      *    orca14（予約）画面
      *****************************************************************
           COPY    "Y01.INC".
           COPY    "Y011.INC".
           COPY    "Y012.INC".
           COPY    "Y03.INC".
           COPY    "Y04.INC".
           COPY    "Y05.INC".
           COPY    "Y06.INC".
           COPY    "YERR.INC".
           COPY    "YID1.INC".
           COPY    "CPAPIMODV2.INC".
      *    ORAPION001R1V2     
           COPY    "CPAPILSTV2.INC".
      *    ORAPION002R1V2     
           COPY    "CPAPILSTV2.INC"
                                   REPLACING   //APILST-//
                                   BY          //APILST02-//.
      *    ORAPION003R1V2     
           COPY    "CPAPILSTV2.INC"
                                   REPLACING   //APILST-//
                                   BY          //APILST03-//.
      *    ORAPION004R1V2     
           COPY    "CPAPILSTV2.INC"
                                   REPLACING   //APILST-//
                                   BY          //APILST04-//.
      *    ORAPION005R1V2     
           COPY    "CPAPILSTV2.INC"
                                   REPLACING   //APILST-//
                                   BY          //APILST05-//.
      *    ORAPION006R1V2
           COPY    "CPAPILSTV2.INC"
                                   REPLACING   //APILST-//
                                   BY          //APILST06-//.
      *    ORAPION007R1V2
           COPY    "CPAPILSTV2.INC"
                                   REPLACING   //APILST-//
                                   BY          //APILST07-//.
      *    ORAPION008R1V2
           COPY    "CPAPILSTV2.INC"
                                   REPLACING   //APILST-//
                                   BY          //APILST08-//.
      *    ORAPION009R1V2
           COPY    "CPAPILSTV2.INC"
                                   REPLACING   //APILST-//
                                   BY          //APILST09-//.
