        01  HCM18V06.
            02  HCM18V06-KANANAME
                        PIC X(50).
            02  HCM18V06-NAME
                        PIC X(50).
            02  HCM18V06-HOSPNAME
                        PIC X(90).
            02  HCM18V06-BYOSYOSU
                        PIC X(4).
            02  HCM18V06-TOTALTEN
                        PIC X(6).
            02  HCM18V06-TOTALMONEY
                        PIC X(10).
            02  HCM18V06-KOFUNUM.
              03  HCM18V06-KOFUNUM-S
                        PIC X(1)        OCCURS 13 TIMES.
            02  HCM18V06-SRYYMD.
              03  HCM18V06-SRYYMD-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCM18V06-KENSAYMD.
              03  HCM18V06-KENSAYMD-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCM18V06-GOKEI.
              03  HCM18V06-GOKEI-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCM18V06-KEIKA-G
                                        OCCURS 3 TIMES.
              03  HCM18V06-KEIKA
                        PIC X(52).
            02  HCM18V06-ZENKAIY
                        PIC X(2).
            02  HCM18V06-ZENKAIM
                        PIC X(2).
            02  HCM18V06-ZENKAID
                        PIC X(2).
            02  HCM18V06-SYOSHINY
                        PIC X(2).
            02  HCM18V06-SYOSHINM
                        PIC X(2).
            02  HCM18V06-SYOSHIND
                        PIC X(2).
            02  HCM18V06-SAISHINY
                        PIC X(2).
            02  HCM18V06-SAISHINM
                        PIC X(2).
            02  HCM18V06-SAISHIND
                        PIC X(2).
            02  HCM18V06-JIKANGAI1
                        PIC X(2).
            02  HCM18V06-JIKANGAI2
                        PIC X(2).
            02  HCM18V06-JIKANGAI3
                        PIC X(2).
            02  HCM18V06-KAISU-G
                                        OCCURS 32 TIMES.
              03  HCM18V06-KAISU
                        PIC X(3).
            02  HCM18V06-KOUTEN-G
                                        OCCURS 32 TIMES.
              03  HCM18V06-KOUTEN
                        PIC X(7).
            02  HCM18V06-SRYKBN-G
                                        OCCURS 30 TIMES.
              03  HCM18V06-SRYKBN
                        PIC X(4).
            02  HCM18V06-TEKIYO-G
                                        OCCURS 30 TIMES.
              03  HCM18V06-TEKIYO
                        PIC X(52).
            02  HCM18V06-SYOBYOEDA
                        PIC X(1).
            02  HCM18V06-SYOBYO
                        PIC X(2).
            02  HCM18V06-SYOSHINMONEY
                        PIC X(7).
            02  HCM18V06-SAISHINMONEY
                        PIC X(7).
            02  HCM18V06-SYOKEI
                        PIC X(7).
            02  HCM18V06-PTNUM
                        PIC X(20).
            02  HCM18V06-JIKANGAI11
                        PIC X(4).
            02  HCM18V06-JIKANGAI21
                        PIC X(4).
            02  HCM18V06-JIKANGAI31
                        PIC X(4).
            02  HCM18V06-RENNUM
                        PIC X(7).
            02  HCM18V06-PAGE
                        PIC X(5).
            02  HCM18V06-LABEL
                        PIC X(14).
            02  HCM18V06-LOPTION
                        PIC X(100).
            02  HCM18V06-ZENKAIG
                        PIC X(1).
            02  HCM18V06-SYOSHING
                        PIC X(1).
            02  HCM18V06-SAISHING
                        PIC X(1).
            02  HCM18V06-HEAD-G.
              03  HCM18V06-HEAD
                        PIC X(62)       OCCURS 2 TIMES.
