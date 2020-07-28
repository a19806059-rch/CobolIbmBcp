CBL LIST,XREF,NUMCHECK(ZON,PAC,BIN)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILECALC.
      * This program reads a file of input values into INVALS-WS
      * The operation read into the W-S structure drives the arithmetic
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVALS
           ASSIGN TO UT-S-INVALS
             ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  INVALS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INVALS-REC.
       01  INVALS-REC  PIC X(80).
       WORKING-STORAGE SECTION.
      * End of File switch
       01  INVALS-EOF               PIC X(01) VALUE SPACE.
       01  INVALS-WS.
           05  OPERATION            PIC X(01).
           05  INVALS-1             PIC  99V99.
           05  INVALS-2             PIC  99.
           05  INVALS-RESULT        PIC S99999V99.
       01  WS-VAR-TRABAJO.
           05  W-POT01              PIC S9(5)V99.
           05  W-POT02              PIC S9(5)V99.

       PROCEDURE DIVISION.
           PERFORM 000-Housekeeping.
           PERFORM 100-Main UNTIL INVALS-EOF = 'Y'.
           PERFORM 900-CLOSE-FILES.
           GOBACK.
      *
       000-Housekeeping.
           INITIALIZE INVALS-WS.
           PERFORM 300-OPEN-FILES.
      * Priming Read
           PERFORM 400-READ-INVALS.
       100-Main.
           IF OPERATION = "A" PERFORM 500-ADD
           ELSE IF OPERATION = "S" PERFORM 600-SUBTRACT
           ELSE IF OPERATION = "M" PERFORM 700-MULTIPLY
           ELSE IF OPERATION = "D" PERFORM 800-DIVIDE
           ELSE PERFORM 850-POTENCIA.
           DISPLAY 'INVALS-RESULT... ' INVALS-RESULT.
           PERFORM 400-Read-INVALS.
       300-Open-Files.
           OPEN INPUT INVALS.
       400-Read-INVALS.
           READ INVALS INTO INVALS-WS
      * Set AT END Switch
               AT END MOVE "Y" TO INVALS-EOF
           END-READ.
       500-ADD.
           ADD INVALS-1, INVALS-2 GIVING INVALS-RESULT.
       600-SUBTRACT.
           SUBTRACT INVALS-2 FROM INVALS-1 GIVING INVALS-RESULT.
       700-MULTIPLY.
           MULTIPLY INVALS-1 BY INVALS-2 GIVING INVALS-RESULT.
       800-DIVIDE.
           DIVIDE INVALS-2 BY INVALS-1 GIVING INVALS-RESULT.
      * LOGICA AGREGADA
       850-POTENCIA.
           COMPUTE W-POT01 = INVALS-1 **  (INVALS-2 / 10).
           COMPUTE W-POT02 = FUNCTION SQRT (INVALS-1).
           MOVE W-POT01 TO INVALS-RESULT.
           DISPLAY 'W-POT01 = ' W-POT01.
           DISPLAY 'W-POT02 = ' W-POT02.
       900-CLOSE-FILES.
           CLOSE INVALS.
