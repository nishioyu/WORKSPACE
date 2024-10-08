      *******************************************************************
      * Project code name "ORCA"
      * úãWZvg\tgiJMA standard receipt softwarej
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
      *****************************************************************
      *  vOC³ð
      * Maj/Min/Rev  C³Ò       út      àe
      *  05.00.00    NACL-½X[  18/06/XX  ü@ÎÇÁ
      *****************************************************************
      ******************************************************************
      *    `ohX|XÛ¯êÏXîñ (êÌ»)
      *                                      COPY (CPMDCXMLV3RE6.INC)
      *         fÃs×àe
      ******************************************************************
       01  XML-MEDICALV3RES6.
            03  MDCRES-CONTEXT             PIC S9(9)   BINARY.
            03  MDCRES-OBJECT              PIC X(36).
            03  MDCRES-MODE                PIC S9(9)   BINARY.
            03  MDCRES-RECORDNAME          PIC X(128).
            03  MDCRES-MEDICALRES.
      *        NGXgÔ
               05  MDCRES-REQUEST-NUMBER    PIC X(2).
      *        X|XÔ
               05  MDCRES-RESPONSE-NUMBER   PIC X(2).
      *        UIDidqJej
               05  MDCRES-KARTE-UID         PIC X(36).
      *        UIDiIJj
               05  MDCRES-ORCA-UID          PIC X(36).
      *        À{ú
               05  MDCRES-INFORMATION-DATE  PIC X(10).
      *        À{Ô
               05  MDCRES-INFORMATION-TIME  PIC X(8).
      *        ÔpR[h
               05  MDCRES-API-RESULT        PIC X(3).
      *        ÔpbZ[W
               05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
               05  MDCRES-RESKEY            PIC X(100).
      *        fÃN
               05  MDCRES-PERFORM-YM        PIC X(07).
      *        fÃÈ
               05  MDCRES-APPOINT-DEP-CODE  PIC X(2).
               05  MDCRES-APPOINT-DEP-NAME  PIC X(80).
      *        ³Òîñ
               05  MDCRES-PATIENTID        PIC X(20).
               05  MDCRES-NAME             PIC X(100).
               05  MDCRES-KANANAME         PIC X(100).
               05  MDCRES-BIRTHDAY         PIC X(10).
               05  MDCRES-SEX              PIC X(1).
      * NGXgOP
      *        ófðîñ
               05  MDCRES-MEDICAL-LIST-G.
                   07  MDCRES-MEDICAL-LIST-TBL     OCCURS   150.
      *                fÃút
                       09  MDCRES-PERFORM-DATE     PIC X(10).
      *                `[Ô
                       09  MDCRES-INVOICD-NUMBER        PIC  X(07).
      *                Û¯g¹Ô
                       09  MDCRES-MED-COMBINATION-NUMBER   PIC X(04).
      *                AÔ
                       09  MDCRES-MED-RENNUM             PIC X(01).
      *                Û¯g¹SiOj
                       09  MDCRES-MED-COMBINATION-RATE  PIC X(03).
      *                Û¯g¹ LøJnú
                       09  MDCRES-MED-START-DATE        PIC X(10).
      *                Û¯g¹ LøI¹ú
                       09  MDCRES-MED-END-DATE          PIC X(10).
      *                Û¯ÌíÞ
                       09  MDCRES-MED-INSURANCE-CLASS   PIC X(03).
      *                Û¯ÌíÞ¼Ì
                       09  MDCRES-MED-INSURANCE-NAME    PIC X(20).
      *                Û¯ÒÔ
                       09  MDCRES-MED-INSURANCE-NUMBER  PIC X(08).
      *                öïîñ
                       09  MDCRES-MED-INSURANCE-INFO 
                                              OCCURS  4 TIMES.
      *                 öïÌíÞ
                        11  MDCRES-MED-INSU-CLASS     PIC X(03).
      *                 öïÌíÞ¼Ì
                        11  MDCRES-MED-INSU-NAME      PIC X(20).
      *                 SÒÔ
                        11  MDCRES-MED-PROVIDER-NUMBER     PIC X(08).
      *                 óÒÔ
                        11  MDCRES-MED-RECIPIENT-NUMBER    PIC X(20).
      *                Û¯g¹íæªiPFíAQFúÔO
      *                                    RFOñ\¦j
                       09  MDCRES-MED-COMBINATION-MODE  PIC X(1).
      *H30.6
      *                ü@²Ü¿ÌÝZè
                       09  MDCRES-CHOZAI-AREA.
                           11  MDCRES-CHOZAI-ONLY      PIC X(10).
      *!!
      * NGXgOP
      *        Û¯g¹êÔp
               05  MDCRES-INSURANCE-INFO-G.
      *           Û¯îñiÛ¯g¹j
                  06  MDCRES-INSURANCE-INFO     OCCURS  100 TIMES.
      *               Û¯g¹Ô
                      07  MDCRES-COMBINATION-NUMBER     PIC X(04).
      *               Û¯g¹S
                      07  MDCRES-COMBINATION-RATE       PIC X(03).
      *               Û¯g¹ LøJnú
                      07  MDCRES-COMB-START-DATE        PIC X(10).
      *               Û¯g¹ LøI¹ú
                      07  MDCRES-COMB-END-DATE          PIC X(10).
      *
      *               Û¯ÌíÞ
                      07  MDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               Û¯ÌíÞ¼Ì
                      07  MDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *               Û¯ÒÔ
                      07  MDCRES-MAIN-INSURANCE-NUMBER   PIC X(08).
      *               L
                      07  MDCRES-MAIN-MARK              PIC X(80).
      *               Ô
                      07  MDCRES-MAIN-NUMBER            PIC X(80).
      *R03.01  }Ô
                      07  MDCRES-MAIN-EDABAN            PIC X(02).
      *               p±æª
                      07  MDCRES-MAIN-CONTINUATION      PIC X(01).
      *               âæª
                      07  MDCRES-MAIN-ASSISTANCE        PIC X(01).
                      07  MDCRES-MAIN-ASSISTANCE-NAME   PIC X(20).
      *               {lÆ°æª
                      07  MDCRES-MAIN-FAMILY            PIC X(01).
      *               Jnú
                      07  MDCRES-MAIN-START-DATE        PIC X(10).
                      07  MDCRES-MAIN-END-DATE          PIC X(10).
      *               öïîñ
                      07  MDCRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *                 öïÌíÞ
                        09  MDCRES-INSURANCE-CLASS         PIC X(03).
      *                 öïÌíÞ¼Ì
                        09  MDCRES-INSURANCE-NAME          PIC X(20).
      *                 SÒÔ
                        09  MDCRES-PROVIDER-NUMBER         PIC X(08).
      *                 óÒÔ
                        09  MDCRES-RECIPIENT-NUMBER        PIC X(20).
      *                 O|S¦ij
                        09  MDCRES-RATE-OUTPATIENT         PIC X(04).
      *                 O|Åèz
                        09  MDCRES-MONEY-OUTPATIENT        PIC X(06).
      *                 Jnú
                        09  MDCRES-INSURANCE-START-DATE    PIC X(10).
                        09  MDCRES-INSURANCE-END-DATE      PIC X(10).
      * NGXgOQ
      *       êÏXàeiNGXgOQj
               05  MDCRES-CHENGE-INFO.
      *           ÏXO@Û¯g¹Ô
                  07  MDCRES-IN-COMBINATION-NUMBER    PIC X(04).
      *           ÏXã@Û¯g¹Ô
                  07  MDCRES-OT-COMBINATION-NUMBER    PIC X(04).
      *           ÏXúÔ
                  07  MDCRES-START-DAY         PIC X(02).
                  07  MDCRES-END-DAY           PIC X(02).
      *           û[êXV è
                  07  MDCRES-INCOME-CHANGE     PIC X(03).
      *        ÏXãAû[îñ
               05  MDCRES-CD-INF-G.
                   07  MDCRES-CD-INF-TBL     OCCURS   100.
      *                fÃút
                       09  MDCRES-CD-PERFORM-DATE       PIC X(10).
      *                `[Ô
                       09  MDCRES-CD-INVOICD-NUMBER    PIC  X(07).
      *                Û¯g¹Ô
                       09  MDCRES-CD-COMBINATION-NUMBER PIC X(04).
      *                û[S
                       09  MDCRES-CD-RATE               PIC X(03).
      *                `[­sút
                       09  MDCRES-CD-DENPPRTYMD         PIC X(10).
      *                ÏXO@¿àz
                       09  MDCRES-MAE-AC-MONEY          PIC X(07).
      *                ÏXã@¿àz
                       09  MDCRES-ATO-AC-MONEY          PIC X(07).
      *                XVsÂæª
                       09  MDCRES-DC-ERR                PIC X(06).
      *        xbZ[W
               05  MDCRES-WARING-G.
                   07  MDCRES-WARING-OCC       OCCURS  20.
                       09  MDCRES-WARNING              PIC X(03).
                       09  MDCRES-WARNIG-MESG          PIC X(200).
      *
