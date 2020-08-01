****** ***************************** Top of Data *************************
000001        IDENTIFICATION DIVISION.                                    
000002        PROGRAM-ID.    HELLOW.                                      
000003        AUTHOR.        RCH.                                         
000004        DATE-WRITTEN.  11/06/2020.                                  
000005       *========================================================    
000006       * PGM EN COBOL v6.3 The idea is to Test all Pitfalls    *    
000007       * identified by IBM experts:                            *    
000008       * PF01: Invalid data in numeric Usage Display Data Items*    
000009       * PF02: Overpopulated Data Items                        *    
000010       * PF03: Parameter/Argument size mismatch                *    
000011       * PF04: Data Items used before a given value            *    
000012       *========================================================    
000013        DATA DIVISION.                                              
000014        WORKING-STORAGE SECTION.                                    
000015        77 WRK-VAR   PIC X(30) VALUE 'INICIO WORKING'.              
000016        77 A1 PIC X(4) VALUE '00 0'.                                
000017        77 A2 REDEFINES A1 PIC 9(4).                                
000018        01  VAR-VARIAS.                                             
000019            02  WRK-RUC  PIC 9(11) VALUE 10090783761.               
000020        COPY GNSWCVRF.                                             
000021        COPY GNSWVVRF.                                             
000022        PROCEDURE DIVISION.                                        
000023            DISPLAY "Hello World...COBOL is still alive and well"  
000024            DISPLAY "COBOL V6.3  PRUEBAS DE COBOL 6.3..........."  
000025            MOVE "INIT WS " TO WRK-VAR                             
000026            IF A2 EQUAL ZERO                                       
000027               DISPLAY 'IT IS ZERO ' A2                            
000028            ELSE                                                   
000029               DISPLAY 'IT IS NON ZEROS... ' A2                    
000030            END-IF                                                 
000031            DISPLAY "COMPILAR..." WRK-VAR                          
000032       *CALL A PGM COBOL VS                                        
000033            MOVE VRF-VAL          TO VRF-CMND.                     
000034            MOVE VRF-ZERO-S       TO VRF-ZERO.                     
000035            MOVE 10               TO VRF-LMAX.                     
000036            MOVE 'S'              TO VRF-SFDV.                     
000037            MOVE VRF-SVRF-1       TO VRF-SVRF.                     
000038            MOVE WRK-RUC          TO VRF-NUME.                     
000039            MOVE WRK-RUC (11:1)   TO VRF-DVRF.                     
000040            CALL 'GNSPPVRF' USING  VRF-VARI              
000041                RETURNING RETURN-CODE                    
000042            END-CALL                                     
000043            DISPLAY 'PASO CALL RETORNO: ' RETURN-CODE.   
000044            DISPLAY 'OTRO RC: ' VRF-CMSG.                
000045            GOBACK.                                      
****** **************************** Bottom of Data ************