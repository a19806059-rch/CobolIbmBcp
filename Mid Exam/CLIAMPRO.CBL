****** ***************************** Top of Data *****************************
000001 CBL LIST,XREF,NUMCHECK(ZON,PAC,BIN),INITCHECK                          
000002        IDENTIFICATION DIVISION.                                        
000003        PROGRAM-ID. CLAIMPRO.                                           
000004       *===============================================================*
000005       * INSURANCE CLAIM PROCESS                                      =*
000006       * AUTHOR: ROBERTO CHIRINOS                                     =*
000007       * DATE  : 30/07/2020                                           =*
000008       *===============================================================*
000009        ENVIRONMENT DIVISION.                                           
000010        INPUT-OUTPUT SECTION.                                           
000011        FILE-CONTROL.                                                   
000012            SELECT INFILE   ASSIGN   TO ICLAIM                          
000013               FILE STATUS  IS  FS-STAT-CLAIM.                          
000014       *                                                                
000015            SELECT OUTFILE  ASSIGN   TO OCLAIM                          
000016               FILE STATUS  IS  FS-STAT-PRINT.                          
000017        DATA DIVISION.                                                  
000018        FILE SECTION.                                                   
000019        FD INFILE                                                       
000020            RECORDING MODE IS F                                         
000021            BLOCK CONTAINS 0 RECORDS                                    
000022            RECORD CONTAINS 80 CHARACTERS                               
000023            DATA RECORD IS CLAIM-RECORD.                                
000024        01  CLAIM-RECORD                         PIC X(80).             
000025        FD OUTFILE                                                      
000026            RECORDING MODE IS F                                         
000027            RECORD CONTAINS 133 CHARACTERS                              
000028            BLOCK CONTAINS 0 RECORDS                                    
000029            DATA RECORD IS CLAIM-REPORT.                                
000030        01  CLAIM-REPORT                         PIC X(133).            
000031       *                                                                
000032        WORKING-STORAGE SECTION.                                        
000033       *Working Variables                                               
000034        01  WS-CONTADORES.                                              
000035            05  WS-COUNT-RECS               PIC S9(8) COMP SYNC VALUE 0.
000036            05  WS-CLAIM-PAID               PIC S9(8) COMP SYNC VALUE 0.
000037            05  WS-NUM-LINES                PIC S9(4) COMP VALUE 61.    
000038        01  WS-CALC-VARS.                                               
000039            05  WS-DEDUCTIBLE-CAL           PIC 9(9)V99 VALUE ZERO.     
000040            05  WS-MAX-COVERAGE-AMOUNT      PIC 9(9)V99 VALUE 999999.99.
000041            05  WS-CLAIM-POLICY             PIC 9(9)V99 VALUE ZERO.     
000042       *Switches Variables & File Status                                
000043        01  SW-SWITCHE-VARS.                                            
000044            05  SW-END-FILE                     PIC X VALUE '0'.        
000045                88 END-FILE                        VALUE '1'.           
000046                88 NOT-END                         VALUE '0'.           
000047            05  SW-DEDUC-MET                    PIC X VALUE 'N'.        
000048                88 DEDUCTIBLE-NOT-MET              VALUE 'N'.           
000049                88 DEDUCTIBLE-MET                  VALUE 'S'.           
000050            05  FS-STAT-CLAIM                   PIC X(02).              
000051                88 CLAIMINPUT-OK                   VALUE '00'.          
000052            05  FS-STAT-PRINT                   PIC X(02).              
000053                88 CLAIMPRINT-OK                   VALUE '00'.          
000054       *Editing Variables                                               
000055        01  WS-EDIT-VARS.                                               
000056            05  REC-KTR-OUT                PIC Z9.                      
000057            05  DATE-VARS                  PIC X(15).                
000058       * CLAIM RECORD                                                
000059         COPY CLAIMREC.                                              
000060       *                                                             
000061       * Lines for Reporting                                         
000062        01  WS-REPORT-LINES.                                         
000063            02  HEADER-01.                                           
000064                05  FILLER                 PIC X VALUE SPACE.        
000065                05  DATE-HEAD01.                                     
000066                    10  DATE-HEAD01-YEAR   PIC X(04).                
000067                    10  FILLER             PIC X VALUE '/'.          
000068                    10  DATE-HEAD01-MONTH  PIC X(02).                
000069                    10  FILLER             PIC X VALUE '/'.          
000070                    10  DATE-HEAD01-DAY    PIC X(02).                
000071                05  FILLER                 PIC X(40) VALUE SPACES.   
000072                05  FILLER                 PIC X(35)                 
000073                    VALUE 'Group Claims Daily Totals'.               
000074            02  HEADER-02.                                           
000075                05  FILLER                 PIC X VALUE SPACE.        
000076                05  FILLER                 PIC X(23) VALUE SPACES.   
000077            02  HEADER-03.                                           
000078                05  FILLER                 PIC X VALUE SPACE.        
000079                05  FILLER                 PIC X(24) VALUE 'POLICY'. 
000080                05  FILLER                 PIC X(09) VALUE 'POLICY'. 
000081                05  FILLER                 PIC X(09) VALUE 'FIRST'.  
000082                05  FILLER                 PIC X(09) VALUE 'LAST'.   
000083                05  FILLER                 PIC X(10) VALUE 'RENEW'.  
000084                05  FILLER                 PIC X(7)  VALUE 'DEDUC'.  
000085                05  FILLER                 PIC X(9)  VALUE 'COPAY'.  
000086                05  FILLER                 PIC X(08) VALUE 'DEDUC'.  
000087                05  FILLER                 PIC X(15) VALUE 'CLAIM'.  
000088                05  FILLER                 PIC X(10) VALUE 'CLAIM'.  
000089            02  HEADER-04.                                           
000090                05  FILLER                 PIC X VALUE SPACE.        
000091                05  FILLER                 PIC X(24) VALUE 'TYPE'.   
000092                05  FILLER                 PIC X(09) VALUE 'NUMBER'. 
000093                05  FILLER                 PIC X(09) VALUE 'NAME'.   
000094                05  FILLER                 PIC X(09) VALUE 'NAME'.   
000095                05  FILLER                 PIC X(10) VALUE 'DATE'.   
000096                05  FILLER                 PIC X(07) VALUE 'MET'.    
000097                05  FILLER                 PIC X(09) VALUE 'PERCENT'.
000098                05  FILLER                 PIC X(08) VALUE 'AMOUNT'. 
000099                05  FILLER                 PIC X(15) VALUE 'AMOUNT'. 
000100                05  FILLER                 PIC X(10) VALUE 'PAID'.   
000101            02  HEADER-05.                                           
000102                05  FILLER                 PIC X VALUE SPACE.        
000103                05  FILLER                 PIC X(23) VALUE ALL '-'.  
000104                05  FILLER                 PIC X VALUE SPACE.        
000105                05  FILLER                 PIC X(08) VALUE ALL '-'.  
000106                05  FILLER                 PIC X VALUE SPACE.        
000107                05  FILLER                 PIC X(08) VALUE ALL '-'.  
000108                05  FILLER                 PIC X VALUE SPACE.        
000109                05  FILLER                 PIC X(08) VALUE ALL '-'.  
000110                05  FILLER                 PIC X VALUE SPACE.        
000111                05  FILLER                 PIC X(10) VALUE ALL '-'.  
000112                05  FILLER                 PIC X VALUE SPACE.        
000113                05  FILLER                 PIC X(05) VALUE ALL '-'.  
000114                05  FILLER                 PIC X VALUE SPACE.        
000115                05  FILLER                 PIC X(07) VALUE ALL '-'.  
000116                05  FILLER                 PIC X VALUE SPACE.        
000117                05  FILLER                 PIC X(08) VALUE ALL '-'.  
000118                05  FILLER                 PIC X VALUE SPACE.        
000119                05  FILLER                 PIC X(14) VALUE ALL '-'.  
000120                05  FILLER                 PIC X VALUE SPACE.        
000121                05  FILLER                 PIC X(10) VALUE ALL '-'.  
000122            02  DETAIL-RECORD.                                       
000123                05  FILLER                  PIC X VALUE SPACE.       
000124                05  POLICY-TYPE-OU          PIC X(23).               
000125                05  FILLER                  PIC X VALUE SPACE.       
000126                05  INSURED-POLICY-NO-OU    PIC X(08).               
000127                05  FILLER                  PIC X VALUE SPACE.       
000128                05  INSURED-FIRST-NAME-OU    PIC X(08).              
000129                05  FILLER                  PIC X VALUE SPACE.       
000130                05  INSURED-LAST-NAME-OU    PIC X(08).               
000131                05  FILLER                  PIC X VALUE SPACE.       
000132                05  RENEW-YEAR-OU           PIC X(04) VALUE SPACES.  
000133                05  FILLER-SLASH-01         PIC X VALUE '/'.          
000134                05  RENEW-MONTH-OU          PIC X(02) VALUE SPACES.   
000135                05  FILLER-SLASH-02         PIC X VALUE '/'.          
000136                05  RENEW-DAY-OU            PIC X(04) VALUE SPACES.   
000137                05  DEDUC-MET-OU            PIC X.                    
000138                05  FILLER                  PIC X(03) VALUE SPACES.   
000139                05  COPAY-PERCENT-OU        PIC Z9.999.               
000140                05  FILLER                  PIC X(03) VALUE SPACES.   
000141                05  DEDUC-AMOUNT-OU         PIC $$$9.                 
000142                05  FILLER                  PIC X(03) VALUE SPACES.   
000143                05  CLAIM-AMOUNT-OU         PIC $,$$$,$$$.99.         
000144                05  FILLER                  PIC X(03) VALUE SPACES.   
000145                05  CLAIM-PAID-OU           PIC $,$$$,$$$.99.         
000146       *                                                              
000147        PROCEDURE DIVISION.                                           
000148            PERFORM 100-OPEN-FILES.                                   
000149            PERFORM 200-CLAIM-PROCESS UNTIL END-FILE.                 
000150            PERFORM 300-TERMINAR.                                     
000151            GOBACK.                                                   
000152       *                                                             
000153        100-OPEN-FILES.                                              
000154            MOVE FUNCTION CURRENT-DATE TO DATE-VARS                  
000155            DISPLAY "Init MID EXAM PROCESS.. date: " DATE-VARS (1:8) 
000156            OPEN INPUT  INFILE                                       
000157            IF NOT CLAIMINPUT-OK                                     
000158               DISPLAY 'ERROR IN OPEN INPUT CLAIM FILE '             
000159               DISPLAY 'FILE STATUS = ' FS-STAT-CLAIM                
000160               GO TO 900-ERROR.                                      
000161       *                                                             
000162            OPEN OUTPUT OUTFILE                                      
000163            IF NOT CLAIMPRINT-OK                                     
000164               DISPLAY 'ERROR IN OPEN OUPUT CLAIM FILE '             
000165               DISPLAY 'FILE STATUS = ' FS-STAT-PRINT                
000166               GO TO 900-ERROR.                                      
000167       *                                                             
000168            PERFORM 240-READ-RECORD                                  
000169            CONTINUE.                                                
000170       *                                                             
000171       * Main Process                                                    
000172       * Assumptions:                                                    
000173       * 1.-A Claim must not be greater the Max coverage amount          
000174       * This max value is $ 999,999.99                                  
000175       * The Claim amount is a data and it is stored in the ICLAIM file  
000176       *                                                                 
000177       * 2.-The report will be written in a file                         
000178       *                                                                 
000179        200-CLAIM-PROCESS.                                               
000180            ADD 1 TO WS-COUNT-RECS                                       
000181            DISPLAY 'POLICY NUMBER ... ' INSURED-POLICY-NO               
000182            IF CLAIM-AMOUNT < WS-MAX-COVERAGE-AMOUNT                     
000183               PERFORM 210-CALCULATE-DEDUCTIBLE                          
000184               PERFORM 220-DECIDE-IF-PAYCLAIM                            
000185            ELSE                                                         
000186               CONTINUE                                                  
000187            END-IF                                                       
000188            PERFORM 240-READ-RECORD                                      
000189            CONTINUE.                                                    
000190       *                                                                
000191       * This Paragraph calculate the Deductible                        
000192       * The deductible is the Policy Amount times the company's        
000193       * deductible % which is a constant .002                          
000194       * Also calculate the amount Claim the Insurer will pay           
000195        210-CALCULATE-DEDUCTIBLE.                                       
000196            COMPUTE WS-DEDUCTIBLE-CAL ROUNDED = .002 * POLICY-AMOUNT    
000197            IF  WS-DEDUCTIBLE-CAL equal POLICY-DEDUCTIBLE-PAID          
000198                COMPUTE CLAIM-AMOUNT-PAID ROUNDED =                     
000199                CLAIM-AMOUNT - (POLICY-COINSURANCE * CLAIM-AMOUNT / 100)
000200                SET DEDUCTIBLE-MET TO TRUE                              
000201            ELSE                                                        
000202                COMPUTE CLAIM-AMOUNT-PAID ROUNDED =                     
000203                CLAIM-AMOUNT - WS-DEDUCTIBLE-CAL                        
000204                             - (POLICY-COINSURANCE * CLAIM-AMOUNT / 100)
000205                SET DEDUCTIBLE-NOT-MET TO TRUE                          
000206            END-IF                                                      
000207            CONTINUE.                                                   
000208       *                                                                
000209       * The Claim is payed only if POLICY-AMOUNT is greater than    
000210       * CLAIM-AMOUNT-PAID                                           
000211       * Otherwise, the Claim is denied                              
000212        220-DECIDE-IF-PAYCLAIM.                                      
000213            IF  POLICY-AMOUNT   >   CLAIM-AMOUNT-PAID                
000214                ADD 1 TO WS-CLAIM-PAID                               
000215                PERFORM 230-WRITE-REPORT                             
000216            END-IF                                                   
000217            CONTINUE.                                                
000218       *                                                             
000219        230-WRITE-REPORT.                                            
000220            EVALUATE POLICY-TYPE                                     
000221              WHEN 1                                                 
000222                MOVE 'EMPLOYER-PRIVATE'    TO POLICY-TYPE-OU         
000223              WHEN 2                                                 
000224                MOVE 'STANDARD MEDICARE'   TO POLICY-TYPE-OU         
000225              WHEN 3                                                 
000226                MOVE 'AFFORDABLE CARE ACT' TO POLICY-TYPE-OU         
000227              WHEN OTHER                                             
000228                MOVE 'INVALID TYPE'        TO POLICY-TYPE-OU          
000229            END-EVALUATE                                              
000230            MOVE INSURED-POLICY-NO         TO INSURED-POLICY-NO-OU    
000231            MOVE INSURED-FIRST-NAME        TO INSURED-FIRST-NAME-OU   
000232            MOVE INSURED-LAST-NAME         TO INSURED-LAST-NAME-OU    
000233            MOVE .002                      TO COPAY-PERCENT-OU        
000234            MOVE SW-DEDUC-MET              TO DEDUC-MET-OU            
000235            MOVE POLICY-DEDUCTIBLE-PAID    TO DEDUC-AMOUNT-OU         
000236            MOVE CLAIM-AMOUNT              TO CLAIM-AMOUNT-OU         
000237            MOVE CLAIM-AMOUNT-PAID         TO CLAIM-PAID-OU           
000238            MOVE POLICY-YEAR               TO RENEW-YEAR-OU           
000239            MOVE POLICY-MONTH              TO RENEW-MONTH-OU          
000240            MOVE POLICY-DAY                TO RENEW-DAY-OU            
000241            IF  WS-NUM-LINES > 60 THEN                                
000242                PERFORM 231-WRITE-TITLES                              
000243            END-IF                                                    
000244            WRITE CLAIM-REPORT         FROM  DETAIL-RECORD            
000245            ADD 1 TO WS-NUM-LINES                                     
000246            CONTINUE.                                                 
000247       *                                                         
000248        231-WRITE-TITLES.                                        
000249            MOVE DATE-VARS (1:4)   TO    DATE-HEAD01-YEAR        
000250            MOVE DATE-VARS (5:2)   TO    DATE-HEAD01-MONTH       
000251            MOVE DATE-VARS (7:2)   TO    DATE-HEAD01-DAY         
000252            WRITE CLAIM-REPORT     FROM  HEADER-01               
000253            WRITE CLAIM-REPORT     FROM  HEADER-02               
000254            WRITE CLAIM-REPORT     FROM  HEADER-03               
000255            WRITE CLAIM-REPORT     FROM  HEADER-04               
000256            WRITE CLAIM-REPORT     FROM  HEADER-05               
000257            MOVE 5                 TO    WS-NUM-LINES            
000258            CONTINUE.                                            
000259       *                                                         
000260        240-READ-RECORD.                                         
000261            READ INFILE                                          
000262               AT END MOVE 1 TO SW-END-FILE                      
000263            NOT AT END                                           
000264               MOVE CLAIM-RECORD TO CLAIM-RECORD-WS              
000265            END-READ                                             
000266            CONTINUE.                                                   
000267       *                                                                
000268        300-TERMINAR.                                                   
000269            MOVE WS-COUNT-RECS      TO REC-KTR-OUT                      
000270            DISPLAY "----------------  "                                
000271            DISPLAY 'Control Counters  '                                
000272            DISPLAY "----------------  "                                
000273            DISPLAY 'Number of Records:  ' REC-KTR-OUT                  
000274            MOVE WS-CLAIM-PAID             TO REC-KTR-OUT               
000275            DISPLAY 'Claim Paid          ' REC-KTR-OUT                  
000276            DISPLAY "END PROGR: CLAIMPRO"                               
000277            CONTINUE.                                                   
000278       *                                                                
000279        900-ERROR.                                                      
000280            GOBACK.                                                     
****** **************************** Bottom of Data ***************************
