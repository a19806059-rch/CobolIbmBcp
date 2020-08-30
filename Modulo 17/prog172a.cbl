      *================================================================*
      *   WorkShop 17.2a                                               *
      *   Input file: U14377.IBM.STDNT.FILE                            *
      *   Modified by: R.CH.  24/Ago/2020                              *
      *================================================================*
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.   PROG172A.   *> Similar al TABLES02
003200*
000300* ************************************************************
      * THIS IS A WORKING EXAMPLE PROGRAM FOR THE EXERCISE IN
      *  MODULE 17 - OF A TWO-DIM TABLE LOAD & SEARCH OPERATION
      *    CURRENTLY INCOMPLETE: 7/4/2020
      * ************************************************************
003300 INSTALLATION.  IBM-BCP.
003400 DATE-WRITTEN.  24-08-2020.
003500 DATE-COMPILED. 24-08-2020.
003600 SECURITY.   NONE.
003700 ENVIRONMENT DIVISION.
003800 CONFIGURATION SECTION.
003900 SOURCE-COMPUTER.   IBM.
004000 OBJECT-COMPUTER.   IBM.
004100 INPUT-OUTPUT SECTION.
004200 FILE-CONTROL.
004300     SELECT STUDENT-FILE   ASSIGN TO UT-S-STDNTCRS
004300            ORGANIZATION IS SEQUENTIAL.
004400     SELECT CREDITS-REPORT ASSIGN TO UT-S-STCRSRPT
004300            ORGANIZATION IS SEQUENTIAL.
004500 DATA DIVISION.
004600 FILE SECTION.
004700 FD  STUDENT-FILE
           RECORDING MODE IS F
004800     LABEL RECORDS ARE STANDARD.
004900 01  STUDENT-RECORD.
005000     05  STUDENT-NAME            PIC X(20).
           05  STUDENT-COURSES.
               10 STUDENT-COURSE-TAB OCCURS 6 TIMES.
                   15  COURSE-NBR      PIC X(7).
                   15  COURSE-GRADE    PIC X(1).
           05  FILLER                  PIC X(12).
006700 FD  CREDITS-REPORT
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
006800     LABEL RECORDS ARE STANDARD.
006900 01  REPORT-LINE-OUT             PIC X(80).
      *
007000 WORKING-STORAGE SECTION.
007100 01  SWITCHES-IN-PROGRAM.
007200     05  SW-END-OF-DATA            PIC X VALUE 'N'.
007300         88  END-OF-DATA                 VALUE 'Y'.
007200     05  SW-MUS-FOUND              PIC X VALUE 'N'.
007300         88  MUS-FOUND                   VALUE 'Y'.
007200     05  SW-STUDENT-FOUND          PIC X VALUE 'N'.
007300         88  STUDENT-FOUND               VALUE 'Y'.
       01  OUT-FILE.
           05 STUDENT-OUT                  PIC X(80).
           05 COURSE-OUT                   PIC X(80).

       01  SUBSCRIPTS-AND-COUNTERS.
           05  CTR-STUDENTS                 PIC 99 VALUE 0.
           05  STUDENT-SUB                  PIC 99 VALUE 0 COMP.
           05  GRADE-ACCUM                  PIC 99 VALUE 0 COMP.
           05  GRADE-ACCUM-MIN              PIC 99 VALUE 0 COMP.
           05  GRADE-ACCUM-MEAN             PIC 99 VALUE 0 COMP.
           05  COURSES-SUB                  PIC 99 VALUE 0 COMP.
           05  NUM-PAGES                    PIC 99 VALUE 0 COMP.
      *
       01  ANOTHER-WORK-VARIABLES.
           05  DATE-VARS                  PIC X(15).
           05  WS-NUM-LINES               PIC S9(5) COMP VALUE 51.
           05  NUM-PAGES-ED               PIC ZZ9.
      *
004900 01  WS-STUDENT-RECORD.
           02  WS-STUDENT-TABLE OCCURS 5 TIMES.
005000       05  WS-STUDENT-NAME            PIC X(20).
             05  WS-STUDENT-COURSES.
               10 WS-STUDENT-COURSE-TAB OCCURS 6 TIMES.
                   15  WS-COURSE-NBR      PIC X(7).
                   15  WS-COURSE-GRADE    PIC X(1).
      *
       01  TWO-DIM-TABLE-VALUES.
           05 ROW1  PIC X(48) VALUE
           'TUBA101BCALC687BSOCS200AALGB124APHYS002BFLUT140C'.
           05 ROW2  PIC X(48) VALUE
           'BIOL201ATRIG551BSHAK213CPSYC234ABIOL002CDRUM310B'.
           05 ROW3  PIC X(48) VALUE
           'POLY555CGEOM231BRLIT560ABIOL136AMECH002BACCO140D'.
           05 ROW4  PIC X(48) VALUE
           'TUBA567ASTAT043CSHOP980BCHEM534BSTT0002AVIOL610A'.
           05 ROW5  PIC X(48) VALUE
           'MEDC522DPIAN003ASPAN760AEBRT164ARUSS002APIAN170A'.
       01  WS-STUDENT-RECORD-RDF REDEFINES TWO-DIM-TABLE-VALUES.
           02  WS-STUDENT-TABLE-RDF OCCURS 5 INDEXED BY ST-IDX.
             05  WS-STUDENT-COURSES-RDF.
               10 WS-STUDENT-COURSE-TAB-RDF OCCURS 6 TIMES
                  INDEXED BY CRS-IDX.
                   15  WS-COURSE-NBR-RDF      PIC X(7).
                   15  WS-COURSE-GRADE-RDF    PIC X(1).
      *
      * Lines for Reporting
        01  WS-REPORT-LINES.
            02  HEADER-01.
                05  FILLER                 PIC X VALUE SPACE.
                05  DATE-HEAD01.
                    10  DATE-HEAD01-YEAR   PIC X(04).
                    10  FILLER             PIC X VALUE '/'.
                    10  DATE-HEAD01-MONTH  PIC X(02).
                    10  FILLER             PIC X VALUE '/'.
                    10  DATE-HEAD01-DAY    PIC X(02).
                05  FILLER                 PIC X(20) VALUE SPACES.
                05  FILLER                 PIC X(40)
                    VALUE 'Students Courses Breakout'.
                05  PAGE-HEAD01            PIC ZZ9.
      *
            02  HEADER-02.
                05  FILLER                      PIC X VALUE SPACE.
                05  FILLER                      PIC X(79) VALUE SPACES.
      *
            02  HEADER-03.
                05  FILLER                      PIC X VALUE SPACE.
                05  FILLER                      PIC X(04) VALUE SPACES.
                05  FILLER                      PIC X(15)
                    VALUE 'Student Name: '.
                05  STUDENT-NAME-HEAD03         PIC X(20).
      *
            02  FOOTER-01.
                05  FILLER                     PIC X VALUE SPACE.
                05  FILLER                     PIC X(50) VALUE ALL '-'.
      *
            02  FOOTER-02.
                05  FILLER                     PIC X VALUE SPACE.
                05  FILLER                     PIC X(45)
                    VALUE ' Total Numer of Students: '.
                05  STUDENT-COUNTER            PIC ZZ9.
      *
            02  FOOTER-03.
                05  FILLER                     PIC X VALUE SPACE.
                05  FILLER                     PIC X(45)
                    VALUE ' Student with highest QPA: '.
                05  STUDENT-HIGH-QPA           PIC ZZ9.
      *
            02  FOOTER-04.
                05  FILLER                     PIC X VALUE SPACE.
                05  FILLER                     PIC X(45)
                    VALUE ' Student with Lowest  QPA: '.
                05  STUDENT-LOWE-QPA           PIC ZZ9.
      *
            02  FOOTER-05.
                05  FILLER                     PIC X VALUE SPACE.
                05  FILLER                     PIC X(45)
                    VALUE ' Average QPA for all students: '.
                05  STUDENT-AVER-QPA           PIC ZZ9.
      *
            02  DETAIL-01.
                05  FILLER                      PIC X VALUE SPACE.
                05  FILLER                      PIC X(04) VALUE SPACES.
                05  FILLER                      PIC X(15)
                    VALUE '      Course: '.
                05  STUDENT-COURSE-DETAIL       PIC X(10).
                05  FILLER                      PIC X(03) VALUE SPACES.
                05  FILLER                      PIC X(07)
                    VALUE 'Grade: '.
                05  STUDENT-GRADE-DETAIL        PIC X(01).
      *
       PROCEDURE DIVISION.
       000-TOP-LEVEL.
           MOVE FUNCTION CURRENT-DATE TO DATE-VARS
           DISPLAY 'INIT PROG PROG172A....DATE: ' DATE-VARS (1:8).
           PERFORM 100-INITIALIZATION.
           PERFORM 200-PROCESS-RECORDS VARYING STUDENT-SUB
                FROM 1 BY 1 UNTIL STUDENT-SUB > 5. *> Only Table
           MOVE 1 TO STUDENT-SUB.
           MOVE FUNCTION MAX(WS-COURSE-GRADE(ALL , ALL) )
                TO GRADE-ACCUM.
      *    MOVE FUNCTION MIN(WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB))
      *         TO GRADE-ACCUM-MIN.
           MOVE FUNCTION MIN(WS-COURSE-GRADE(ALL , ALL) )
                TO GRADE-ACCUM-MIN.
           COMPUTE GRADE-ACCUM-MEAN ROUNDED = (GRADE-ACCUM+
                                              GRADE-ACCUM-MIN) / 2.
           PERFORM 300-TABLE-SEARCH.
           PERFORM 900-WRAP-UP.
           GOBACK.
      *
010700 100-INITIALIZATION.
010800     OPEN INPUT  STUDENT-FILE.
010900     OPEN OUTPUT CREDITS-REPORT.
      * The Student file Goes to WS-STUDENT-TABLE
            PERFORM  VARYING TALLY
             FROM 1 BY 1
             UNTIL TALLY > 5 OR END-OF-DATA
             READ STUDENT-FILE
               AT END
                  SET END-OF-DATA TO TRUE
               NOT AT END
                 MOVE STUDENT-RECORD TO WS-STUDENT-TABLE(TALLY)
                 DISPLAY 'RECORD: ' STUDENT-RECORD
                 ADD 1 TO CTR-STUDENTS
             END-READ
            END-PERFORM
            CONTINUE.
      *
011400 200-PROCESS-RECORDS.
           MOVE WS-STUDENT-NAME (STUDENT-SUB) TO
                STUDENT-NAME-HEAD03
           IF  WS-NUM-LINES > 50 THEN
               PERFORM 551-WRITE-TITLES
           END-IF
           WRITE REPORT-LINE-OUT  FROM  HEADER-03
           ADD 1 TO WS-NUM-LINES
           PERFORM VARYING COURSES-SUB FROM 1 BY 1
                UNTIL COURSES-SUB > 6        *> (5,6) Table
              EVALUATE WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                  WHEN 'A' MOVE '4' TO
                    WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                  WHEN 'B' MOVE '3' TO
                    WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                  WHEN 'C' MOVE '2' TO
                    WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                  WHEN 'D' MOVE '1' TO
                    WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                  WHEN 'F' MOVE '0' TO
                    WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                  WHEN OTHER MOVE '0' TO
                    WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
              END-EVALUATE
              IF  WS-NUM-LINES > 50 THEN
                  PERFORM 551-WRITE-TITLES
              END-IF
              MOVE WS-COURSE-NBR  (STUDENT-SUB, COURSES-SUB) TO
                   STUDENT-COURSE-DETAIL
              MOVE WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB) TO
                   STUDENT-GRADE-DETAIL
              WRITE REPORT-LINE-OUT FROM DETAIL-01
              ADD 1 TO WS-NUM-LINES
           END-PERFORM
           CONTINUE.
      *
014800 300-TABLE-SEARCH.
           MOVE 3 TO STUDENT-SUB.
           MOVE WS-STUDENT-TABLE(STUDENT-SUB) TO  STUDENT-OUT.
           MOVE 2 TO COURSES-SUB.
           MOVE WS-STUDENT-NAME(STUDENT-SUB) TO STUDENT-OUT.
           MOVE  WS-COURSE-GRADE (STUDENT-SUB, COURSES-SUB)
                        TO COURSE-OUT.
      * Inline PERFORM/VARYING subscript pattern
           PERFORM VARYING STUDENT-SUB FROM 1 BY 1 UNTIL
              STUDENT-SUB > 5 OR STUDENT-FOUND
              PERFORM VARYING COURSES-SUB FROM 1 BY 1
                UNTIL STUDENT-SUB > 5 OR STUDENT-FOUND
                 IF WS-COURSE-NBR (STUDENT-SUB, COURSES-SUB) = 'ANTH101'
                  AND WS-COURSE-GRADE (STUDENT-SUB, COURSES-SUB) = '3'
                        MOVE WS-STUDENT-NAME(STUDENT-SUB) TO STUDENT-OUT
                        MOVE 'Y' TO SW-STUDENT-FOUND
                 END-IF
              END-PERFORM
           END-PERFORM.
      * Indexed SEARCH code pattern
           PERFORM VARYING ST-IDX FROM 1 BY 1
               UNTIL ST-IDX > 5 OR MUS-FOUND
           SET CRS-IDX TO 1
      *  Find the first TUBA student with an "A" in TUBA567
           SEARCH WS-STUDENT-COURSE-TAB-RDF
           WHEN (WS-COURSE-NBR-RDF (ST-IDX, CRS-IDX)  = 'TUBA567'
                AND WS-COURSE-GRADE-RDF (ST-IDX, CRS-IDX) = 'A')
                OR ( WS-COURSE-NBR-RDF (ST-IDX, CRS-IDX) = 'PIANO003'
                AND WS-COURSE-GRADE-RDF (ST-IDX, CRS-IDX) = 'A')
                DISPLAY 'In Search *** Musician Found ***'
                MOVE 'Y' TO SW-MUS-FOUND
           END-SEARCH
           END-PERFORM.
      *
       551-WRITE-TITLES.
           ADD 1 TO NUM-PAGES
           MOVE NUM-PAGES         TO    NUM-PAGES-ED PAGE-HEAD01
           MOVE DATE-VARS (1:4)   TO    DATE-HEAD01-YEAR
           MOVE DATE-VARS (5:2)   TO    DATE-HEAD01-MONTH
           MOVE DATE-VARS (7:2)   TO    DATE-HEAD01-DAY
           WRITE REPORT-LINE-OUT  FROM  HEADER-01
           WRITE REPORT-LINE-OUT  FROM  HEADER-02
           MOVE 2                 TO    WS-NUM-LINES
           CONTINUE.

       900-WRAP-UP.
           MOVE CTR-STUDENTS       TO  STUDENT-COUNTER
           MOVE GRADE-ACCUM        TO  STUDENT-HIGH-QPA
           MOVE GRADE-ACCUM-MIN    TO  STUDENT-LOWE-QPA
           MOVE GRADE-ACCUM-MEAN   TO  STUDENT-AVER-QPA
           WRITE REPORT-LINE-OUT FROM FOOTER-01
           WRITE REPORT-LINE-OUT FROM FOOTER-02
           WRITE REPORT-LINE-OUT FROM FOOTER-03
           WRITE REPORT-LINE-OUT FROM FOOTER-04
           WRITE REPORT-LINE-OUT FROM FOOTER-05
           DISPLAY 'END PROG PROG172A ...'
015200     CLOSE CREDITS-REPORT  STUDENT-FILE.
