        01  HCM12V03.
            02  HCM12V03-TEISYUTUSAKI
                        PIC X(6).
            02  HCM12V03-PTNUM
                        PIC X(20).
            02  HCM12V03-SRYGG
                        PIC X(4).
            02  HCM12V03-SRYY
                        PIC X(2).
            02  HCM12V03-SRYM
                        PIC X(2).
            02  HCM12V03-PREFNUM
                        PIC X(2).
            02  HCM12V03-HOSPCDN
                        PIC X(20).
            02  HCM12V03-NAIYO-TBL
                                        OCCURS 140 TIMES.
              03  HCM12V03-SRYKBN
                        PIC X(2).
              03  HCM12V03-TEKIYO
                        PIC X(52).
            02  HCM12V03-RJNFTNJANUM.
              03  HCM12V03-RJNFTNJANUMS
                        PIC X(1)        OCCURS 8 TIMES.
            02  HCM12V03-KOHFTNJANUM1.
              03  HCM12V03-KOHFTNJANUM1S
                        PIC X(1)        OCCURS 8 TIMES.
            02  HCM12V03-KOHFTNJANUM2.
              03  HCM12V03-KOHFTNJANUM2S
                        PIC X(1)        OCCURS 8 TIMES.
            02  HCM12V03-KOHFTNJANUM1N
                        PIC X(20).
            02  HCM12V03-KOHFTNJANUM2N
                        PIC X(20).
            02  HCM12V03-HKNJANUM.
              03  HCM12V03-HKNJANUMS
                        PIC X(1)        OCCURS 8 TIMES.
            02  HCM12V03-KYURATE
                        PIC X(8).
            02  HCM12V03-RJNJKYSNUM.
              03  HCM12V03-RJNJKYSNUMS
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCM12V03-KOHJKYSNUM1.
              03  HCM12V03-KOHJKYSNUM1S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCM12V03-KOHJKYSNUM2.
              03  HCM12V03-KOHJKYSNUM2S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCM12V03-KOHJKYSNUM1N
                        PIC X(20).
            02  HCM12V03-KOHJKYSNUM2N
                        PIC X(20).
            02  HCM12V03-HKNSBT1
                        PIC X(6).
            02  HCM12V03-HKNSBT2
                        PIC X(6).
            02  HCM12V03-HONKZK
                        PIC X(10).
            02  HCM12V03-KIGO
                        PIC X(34).
            02  HCM12V03-NUM
                        PIC X(34).
            02  HCM12V03-RENNUM
                        PIC X(7).
            02  HCM12V03-NAME
                        PIC X(44).
            02  HCM12V03-TYTLE
                        PIC X(14).
            02  HCM12V03-PAGE
                        PIC X(5).
            02  HCM12V03-MOJI1
                        PIC X(10).
            02  HCM12V03-MOJI2
                        PIC X(10).
            02  HCM12V03-BTUNUM
                        PIC X(9).
            02  HCM12V03-LABEL
                        PIC X(14).
            02  HCM12V03-LOPTION
                        PIC X(100).
            02  HCM12V03-KIGO2
                        PIC X(34).
            02  HCM12V03-NUM2
                        PIC X(34).
            02  HCM12V03-EDABANNAME
                        PIC X(8).
            02  HCM12V03-EDABAN
                        PIC X(2).
