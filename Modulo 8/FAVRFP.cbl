CBL LIST,XREF,NUMCHECK(ZON,PAC,BIN),INITCHECK
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRFP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE   ASSIGN   TO RFPIN.
           SELECT OUTFILE  ASSIGN   TO RFPOUT.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS RFPIN-REC.
       01  RFPIN-REC.
           05  ARTIST-ACCT-NO                 PIC X(08).
           05  ARTIST-MUSICAL-GENRE           PIC X(06).
               88  ROCK                           VALUE 'ROCK'.
               88  JAZZ                           VALUE 'JAZZ'.
               88  FUSION                         VALUE 'FUSION'.
           05  MUSICIAN.
               10  MUSICIAN-LNAME             PIC X(15).
               10  MUSICIAN-FNAME             PIC X(15).
           05  MUSICAL-INSTRUMENT-TYPE        PIC X(06).
           05  INSTRUMENT-QUALITY             PIC X(01).
               88  USED-FLAG                      VALUE 'U'.
               88  NEW-FLAG                       VALUE 'N'.
               88  PREMIUM-FLAG                   VALUE 'P'.
           05  MAX-MUSICIAN-BUDGET-AMOUNT     PIC 9(5)V99.
           05  SHIP-TO                        PIC X(03).
               88  IN-COUNTRY                     VALUE "IN".
               88  OUT-OF-COUNTRY                 VALUE 'OUT'.
           05  FILLER                         PIC X(19).
       FD OUTFILE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PROP-REC.
       01  PROP-REC.
           05  ARTIST-ACCT-NO-O               PIC X(08).
           05  ARTIST-MUSICAL-GENRE-O         PIC X(06).
               88  ROCK-O                         VALUE 'ROCK'.
               88  JAZZ-O                         VALUE 'JAZZ'.
               88  FUSION-O                       VALUE 'FUSION'.
           05  MUSICIAN-O.
               10  MUSICIAN-LNAME-O           PIC X(15).
               10  MUSICIAN-FNAME-O           PIC X(15).
           05  MUSICAL-INSTRUMENT-TYPE-O      PIC X(06).
           05  INSTRUMENT-QUALITY-O           PIC X(01).
               88  USED-FLAG-O                    VALUE 'U'.
               88  NEW-FLAG-O                     VALUE 'N'.
               88  PREMIUM-FLAG-O                 VALUE 'P'.
           05  SHIP-TO-O                      PIC X(03).
               88  IN-COUNTRY-O                   VALUE "IN".
               88  OUT-OF-COUNTRY-O               VALUE 'OUT'.
           05  COST-PER-INSTRUMENT-O          PIC S9(7)V99.
           05  ADDITIONAL-COSTS-O.
               10  SHIPPING-COST-O            PIC S9(4)V99.
               10  TAX-O                      PIC S9(3)V99.
           05  FILLER                         PIC X(03).
       WORKING-STORAGE SECTION.
      *Working Variables
       01  WS-CONTADORES.
           05  WS-CONTAR                 PIC S9(8) COMP SYNC.
           05  WS-CONTAR-Z               PIC  9(3) VALUE 0.
           05  WS-HIGH-COST              PIC 9(3)V99 VALUE 0.
           05  WS-LOW-COST               PIC 9(3)V99 VALUE 0.
           05  WS-KEYBOARD-COST          PIC  9(7)V99 VALUE 3017.89.
           05  WS-VOCALS-COST            PIC  9(7)V99 VALUE 599.05.
           05  WS-GUITAR-COST            PIC  9(7)V99 VALUE 2648.99.
           05  WS-BASS-COST              PIC  9(7)V99 VALUE 1871.
           05  WS-DRUM-COST              PIC  9(7)V99 VALUE 3087.22.
           05  WS-PERCUSSION-COST        PIC  9(7)V99 VALUE 799.99.
       77  WS-COST-AVER                  PIC 9(3)V99.
       77  WS-COST                       PIC 9(3)V99.
       77  WE-COST                       PIC $ZZ,ZZ9.99.
       77  SW-END                        PIC 9 VALUE 0.
           88  NOT-EOF                         VALUE 0.
           88  EOF-FILE                        VALUE 1.
      *Editing Variables
       01  WS-EDIT-VARS.
           05  REC-KTR                    PIC 99 VALUE ZERO.
           05  COST-TOTAL                 PIC 9(5)V99 VALUE ZERO.
           05  REC-KTR-OUT                PIC Z9.
           05  COST-TOTAL-OUT             PIC $Z99.99.
           05  AVER-COST                  PIC $Z99.99.
       PROCEDURE DIVISION.
           PERFORM OPEN-FILES.
           PERFORM CALCULUS UNTIL EOF-FILE.
           PERFORM TERMINAR.
           GOBACK.
           
      *
       OPEN-FILES.
           DISPLAY "Init FAVRFP..".
           OPEN INPUT  INFILE.
           OPEN OUTPUT OUTFILE.
           READ INFILE
           AT END
               MOVE 1 TO SW-END
           NOT AT END
               SET NOT-EOF    TO TRUE
           END-READ.
      *
       CALCULUS.
           ADD 1 TO WS-CONTAR
           MOVE WS-CONTAR   TO WS-CONTAR-Z
           EVALUATE TRUE
             WHEN  MUSICAL-INSTRUMENT-TYPE = 'KEYBOARD'
TAX            COMPUTE TAX-O ROUNDED = WS-KEYBOARD-COST * 8 / 100
               EVALUATE TRUE
                 WHEN  INSTRUMENT-QUALITY = 'U'
                  IF IN-COUNTRY
                   COMPUTE SHIPPING-COST-O ROUNDED =
                   0.1 *  WS-KEYBOARD-COST
                   COMPUTE WS-COST ROUNDED =
                          WS-KEYBOARD-COST * (1 - 20 / 100  ) +
                          TAX-O + SHIPPING-COST-O
                  ELSE
                   COMPUTE SHIPPING-COST-O ROUNDED =
                   0.2 *  WS-KEYBOARD-COST
                   COMPUTE WS-COST ROUNDED =
                          WS-KEYBOARD-COST * (1 - 20 / 100  ) +
                          TAX-O + SHIPPING-COST-O
                  END-IF
                 WHEN  INSTRUMENT-QUALITY = 'N'
                  IF IN-COUNTRY
                   COMPUTE SHIPPING-COST-O ROUNDED =
                   0.1 *  WS-KEYBOARD-COST
                   COMPUTE WS-COST ROUNDED =
                           WS-KEYBOARD-COST                    +
                           TAX-O + SHIPPING-COST-O
                  ELSE
                   COMPUTE SHIPPING-COST-O ROUNDED =
                   0.2 *  WS-KEYBOARD-COST
                   COMPUTE WS-COST ROUNDED =
                           WS-KEYBOARD-COST                    +
                           TAX-O + SHIPPING-COST-O
                  END-IF
                 WHEN  INSTRUMENT-QUALITY = 'P'
                  IF IN-COUNTRY
                   COMPUTE SHIPPING-COST-O ROUNDED =
                   0.1 *  WS-KEYBOARD-COST
                   COMPUTE WS-COST ROUNDED =
                           WS-KEYBOARD-COST * (1 + 20 / 100 )  +
                           TAX-O + SHIPPING-COST-O
                  ELSE
                   COMPUTE SHIPPING-COST-O ROUNDED =
                   0.2 *  WS-KEYBOARD-COST
                   COMPUTE WS-COST ROUNDED =
                           WS-KEYBOARD-COST * (1 + 20 / 100 )  +
                           TAX-O + SHIPPING-COST-O
                  END-IF
               END-EVALUATE
             WHEN  MUSICAL-INSTRUMENT-TYPE = 'VOCALS'
               MOVE WS-VOCALS-COST TO WS-KEYBOARD-COST
TAX            COMPUTE TAX-O ROUNDED = WS-KEYBOARD-COST * 8 / 100
               EVALUATE TRUE
                 WHEN  INSTRUMENT-QUALITY = 'U'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                           WS-KEYBOARD-COST * (1 - 20 / 100 )  +
                           TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                           WS-KEYBOARD-COST * (1 - 20 / 100 )  +
                           TAX-O + SHIPPING-COST-O
                  END-IF
                 WHEN  INSTRUMENT-QUALITY = 'N'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST                    +
                             TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST                    +
                             TAX-O + SHIPPING-COST-O
                  END-IF
                 WHEN  INSTRUMENT-QUALITY = 'P'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST * (1 + 20 / 100 )  +
                             TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST * (1 + 20 / 100 )  +
                             TAX-O + SHIPPING-COST-O
                  END-IF
               END-EVALUATE
             WHEN  MUSICAL-INSTRUMENT-TYPE = 'GUITAR'
               MOVE WS-GUITAR-COST TO WS-KEYBOARD-COST
TAX            COMPUTE TAX-O ROUNDED = WS-KEYBOARD-COST * 8 / 100
               EVALUATE TRUE
                 WHEN  INSTRUMENT-QUALITY = 'U'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                           WS-KEYBOARD-COST * (1 - 20 / 100 )  +
                           TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                           WS-KEYBOARD-COST * (1 - 20 / 100 )  +
                           TAX-O + SHIPPING-COST-O
                  END-IF
                 WHEN  INSTRUMENT-QUALITY = 'N'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST                    +
                             TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST                    +
                             TAX-O + SHIPPING-COST-O
                  END-IF
                 WHEN  INSTRUMENT-QUALITY = 'P'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST * (1 + 20 / 100 )  +
                             TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST * (1 + 20 / 100 )  +
                             TAX-O + SHIPPING-COST-O
                  END-IF
               END-EVALUATE
             WHEN  MUSICAL-INSTRUMENT-TYPE = 'BASS'
               MOVE WS-BASS-COST   TO WS-KEYBOARD-COST
TAX            COMPUTE TAX-O ROUNDED = WS-KEYBOARD-COST * 8 / 100
               EVALUATE TRUE
                 WHEN  INSTRUMENT-QUALITY = 'U'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                           WS-KEYBOARD-COST * (1 - 20 / 100 )  +
                           TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                           WS-KEYBOARD-COST * (1 - 20 / 100 )  +
                           TAX-O + SHIPPING-COST-O
                  END-IF
                 WHEN  INSTRUMENT-QUALITY = 'N'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST                    +
                             TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST                    +
                             TAX-O + SHIPPING-COST-O
                  END-IF
                 WHEN  INSTRUMENT-QUALITY = 'P'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST * (1 + 20 / 100 )  +
                             TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST * (1 + 20 / 100 )  +
                             TAX-O + SHIPPING-COST-O
                  END-IF
               END-EVALUATE
             WHEN  MUSICAL-INSTRUMENT-TYPE = 'DRUMS'
               MOVE WS-DRUM-COST   TO WS-KEYBOARD-COST
TAX            COMPUTE TAX-O ROUNDED = WS-KEYBOARD-COST * 8 / 100
               EVALUATE TRUE
                 WHEN  INSTRUMENT-QUALITY = 'U'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                           WS-KEYBOARD-COST * (1 - 20 / 100 )  +
                           TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                           WS-KEYBOARD-COST * (1 - 20 / 100 )  +
                           TAX-O + SHIPPING-COST-O
                  END-IF
                 WHEN  INSTRUMENT-QUALITY = 'N'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST                    +
                             TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST                    +
                             TAX-O + SHIPPING-COST-O
                  END-IF
                 WHEN  INSTRUMENT-QUALITY = 'P'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST * (1 + 20 / 100 )  +
                             TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST * (1 + 20 / 100 )  +
                             TAX-O + SHIPPING-COST-O
                  END-IF
               END-EVALUATE
             WHEN  MUSICAL-INSTRUMENT-TYPE = 'PERCUSSION'
               MOVE WS-PERCUSSION-COST     TO WS-KEYBOARD-COST
TAX            COMPUTE TAX-O ROUNDED = WS-KEYBOARD-COST * 8 / 100
               EVALUATE TRUE
                 WHEN  INSTRUMENT-QUALITY = 'U'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                           WS-KEYBOARD-COST * (1 - 20 / 100 )  +
                           TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                           WS-KEYBOARD-COST * (1 - 20 / 100 )  +
                           TAX-O + SHIPPING-COST-O
                  END-IF
                 WHEN  INSTRUMENT-QUALITY = 'N'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST                    +
                             TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST                    +
                             TAX-O + SHIPPING-COST-O
                  END-IF
                 WHEN  INSTRUMENT-QUALITY = 'P'
                  IF IN-COUNTRY
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.1 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST * (1 + 20 / 100 )  +
                             TAX-O + SHIPPING-COST-O
                  ELSE
                     COMPUTE SHIPPING-COST-O ROUNDED =
                     0.2 *  WS-KEYBOARD-COST
                     COMPUTE WS-COST ROUNDED =
                             WS-KEYBOARD-COST * (1 + 20 / 100 )  +
                             TAX-O + SHIPPING-COST-O
                  END-IF
               END-EVALUATE
           END-EVALUATE
           MOVE WS-COST   TO WE-COST.
           ADD WS-COST TO COST-TOTAL GIVING COST-TOTAL.
           PERFORM ESCRIBIR-SALIDA.
           PERFORM LISTAR.
           READ INFILE
           AT END MOVE 1 TO SW-END.
      *
       ESCRIBIR-SALIDA.
           MOVE ARTIST-ACCT-NO          TO ARTIST-ACCT-NO-O
           MOVE ARTIST-MUSICAL-GENRE    TO ARTIST-MUSICAL-GENRE-O
           MOVE MUSICIAN                TO MUSICIAN-O
           MOVE MUSICAL-INSTRUMENT-TYPE TO MUSICAL-INSTRUMENT-TYPE-O
           MOVE INSTRUMENT-QUALITY      TO INSTRUMENT-QUALITY-O
           MOVE SHIP-TO                 TO SHIP-TO-O
           MOVE WS-COST                 TO COST-PER-INSTRUMENT-O
           WRITE PROP-REC.
      *
       LISTAR.
           DISPLAY 'REGISTR No:   '         WS-CONTAR-Z.
           DISPLAY 'ARTIST-ACCT-NO: '       ARTIST-ACCT-NO
           DISPLAY 'ARTIST-MUSICAL-GENRE: ' ARTIST-MUSICAL-GENRE
           DISPLAY 'TOTAL COST:   '         WE-COST.
      *    DISPLAY "END PROGR: FAVRPT".
      *
       TERMINAR.
           MOVE WS-CONTAR          TO REC-KTR-OUT
           MOVE COST-TOTAL         TO COST-TOTAL-OUT
           DISPLAY "----------------  "
           DISPLAY 'Final Statistics: '
           DISPLAY "----------------  "
           DISPLAY 'Number or Records:  ' REC-KTR-OUT
           DISPLAY "END PROGR: FAVRPT".
