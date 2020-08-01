CBL LIST,XREF,NUMCHECK(ZON,PAC,BIN),INITCHECK
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRPT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE   ASSIGN   TO FAVIN.
           SELECT OUTFILE  ASSIGN   TO FAVOUT.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS FAVIN-REC.
       01  FAVIN-REC.
           05  ARTIST-NAME                PIC X(30).
           05  NUMBER-MUSICIANS           PIC 99.
           05  MUSICAL-GENRE              PIC X(12).
           05  COST-TOT.
               10  CD-COST                PIC 99V99.
               10  TAX                    PIC V99.
               10  SHIPPING-COST          PIC 99V99.
               10  BAND-IS-STILL-TOGETHER PIC X.
       FD OUTFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS FAVIN-REC.
       01  FAVOUT-REC.
           05 OUT-FILLER                      PIC X(55).
           05 TOTAL-COST                      PIC 9999V99.
       WORKING-STORAGE SECTION.
      *Working Variables
       01  WS-CONTADORES.
           05  WS-CONTAR                      PIC S9(8) COMP SYNC.
           05  WS-CONTAR-Z                    PIC  9(3) VALUE 0.
           05  WS-HIGH-COST                   PIC 9(3)V99 VALUE 0.
           05  WS-LOW-COST                    PIC 9(3)V99 VALUE 0.
       77  WS-COST-AVER                       PIC 9(3)V99.
       77  WS-COST                            PIC 9(3)V99.
       77  WE-COST                            PIC ZZZ9.99.
       77  SW-END                             PIC X VALUE '0'.
      *Editing Variables
       01  WS-EDIT-VARS.
           05  REC-KTR                    PIC 99 VALUE ZERO.
           05  COST-TOTAL                 PIC 9(5)V99 VALUE ZERO.
           05  REC-KTR-OUT                PIC Z9.
           05  COST-TOTAL-OUT             PIC $Z99.99.
           05  AVER-COST                  PIC $Z99.99.
       PROCEDURE DIVISION.
           PERFORM OPEN-FILES.
           PERFORM CALCULUS UNTIL SW-END = '1'.
           PERFORM TERMINAR.
           GOBACK.
      *
       OPEN-FILES.
           DISPLAY "Init FAVRPT..".
           OPEN INPUT  INFILE.
           OPEN OUTPUT OUTFILE.
           READ INFILE
           AT END MOVE 1 TO SW-END
           NOT AT END
            COMPUTE WS-COST = CD-COST * (1 + TAX / 100 ) + SHIPPING-COST
            MOVE WS-COST  TO WS-HIGH-COST WS-LOW-COST
           END-READ.
      *
       CALCULUS.
           ADD 1 TO WS-CONTAR
           MOVE WS-CONTAR   TO WS-CONTAR-Z
           COMPUTE WS-COST = CD-COST * (1 + TAX / 100 ) + SHIPPING-COST.
           MOVE WS-COST   TO WE-COST.
           ADD WS-COST TO COST-TOTAL GIVING COST-TOTAL.
      * Find the Highest & Lowest CD cost
           IF  WS-HIGH-COST > WS-COST
               CONTINUE
           ELSE
               MOVE WS-COST TO WS-HIGH-COST
           END-IF
           IF  WS-LOW-COST < WS-COST
               CONTINUE
           ELSE
               MOVE WS-COST TO WS-LOW-COST
           END-IF.
           PERFORM LISTAR.
           PERFORM ESCRIBIR-SALIDA.
           READ INFILE
           AT END MOVE 1 TO SW-END.
      *
       ESCRIBIR-SALIDA.
           MOVE FAVIN-REC TO OUT-FILLER.
           MOVE WS-COST   TO TOTAL-COST.
           WRITE FAVOUT-REC.
      *
       LISTAR.
           DISPLAY 'REGISTR No: '  WS-CONTAR-Z.
           DISPLAY 'TOTAL COST: '  WE-COST.
      *    DISPLAY "END PROGR: FAVRPT".
      *
       TERMINAR.
           MOVE WS-CONTAR          TO REC-KTR-OUT
           MOVE COST-TOTAL         TO COST-TOTAL-OUT
           DIVIDE COST-TOTAL BY WS-CONTAR GIVING WS-COST-AVER
           COMPUTE AVER-COST  ROUNDED = WS-COST-AVER
           DISPLAY "-----------------  "
           DISPLAY 'Final Statistics  : '
           DISPLAY "-----------------  "
           DISPLAY 'Number or Records:  ' REC-KTR-OUT
           DISPLAY 'Gross Revenue:      ' COST-TOTAL-OUT
           DISPLAY 'Average CD Sale:    ' AVER-COST
           MOVE WS-LOW-COST TO AVER-COST
           DISPLAY 'Lowest Cost:        ' AVER-COST
           MOVE WS-HIGH-COST TO AVER-COST
           DISPLAY 'Highest Cost:       ' AVER-COST
           DISPLAY "END PROGR: FAVRPT".
