       IDENTIFICATION DIVISION.
       PROGRAM-ID. PR3FA21.
       AUTHOR. D REYNOLDS.
      *************************************************************** 
      * THIS PROGRAM READS THE INPUT FILE AND WRITES THREE DIFFERENT 
      * TYPES OF OUTPUT FILES: INVENTORY, EMPLOYEE AND ERROR. 
      * 
      * INPUT WAREHOUSE ID IS CONVERTED USING WAREHOUSE-TABLE IN
      * WORKING STORAGE WHICH IS DETAILED BELOW:
      *    BHAM = B100
      *    HUNT = B200
      *    ANNI = B300
      *    MONT = B400
      *
      * THERE WILL BE NINE OUTPUT FILES:
      *    1. FOUR INVENTORY FILES, ONE FOR EACH WAREHOUSE ID
      *    2. FOUR EMPLOYEE FILES, ONE FOR EACH WAREHOUSE ID
      *    3. ERROR FILE WILL BE WRITTEN, IF WAREHOUSE ID IS NOT FOUND
      *       IN WAREHOUSE-TABLE TO CONVERT TO THE NEW WAREHOUSE ID
      *************************************************************** 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT MESS-FILE
               ASSIGN TO 'PR3FA21-MESS.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT INVENTORY-B100-FILE
               ASSIGN TO 'PR3FA21-INV-B100.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT INVENTORY-B200-FILE
               ASSIGN TO 'PR3FA21-INV-B200.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT INVENTORY-B300-FILE
               ASSIGN TO 'PR3FA21-INV-B300.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT INVENTORY-B400-FILE
               ASSIGN TO 'PR3FA21-INV-B400.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT EMPLOYEE-B100-FILE
               ASSIGN TO 'PR3FA21-EMP-B100.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT EMPLOYEE-B200-FILE
               ASSIGN TO 'PR3FA21-EMP-B200.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT EMPLOYEE-B300-FILE
               ASSIGN TO 'PR3FA21-EMP-B300.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT EMPLOYEE-B400-FILE
               ASSIGN TO 'PR3FA21-EMP-B400.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERROR-FILE 
               ASSIGN TO 'PR3FA21-ERR.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.

       FD  MESS-FILE
           RECORD CONTAINS 224 CHARACTERS.

       01  MESS-RECORD.
           05  MR-WAREHOUSE-ID              PIC X(04).
           05  MR-EMPLOYEE-ID               PIC X(05).
           05  MR-EMPLOYEE-POSITION         PIC X(02).
           05  MR-EMPLOYEE-LAST-NAME        PIC X(10).
           05  MR-EMPLOYEE-FIRST-NAME       PIC X(10).
           05  MR-EMPLOYEE-MID-INITIAL      PIC X(01).
           05                               PIC X(02).
           05  MR-HIRE-DATE                 PIC 9(08).
           05                               PIC X(25).
           05  MR-CURRENT-YEARLY-SALARY     PIC 9(06)V99.
           05  MR-NUMBER-OF-DEPENDENTS      PIC 9(02).
           05  MR-HEALTH-PLAN               PIC X(01).
           05  MR-HEALTH-INS-COST           PIC 9(03).
           05                               PIC X(04).
           05  MR-VENDOR-ID                 PIC X(01).
           05  MR-CANDY-ID                  PIC X(03).
           05  MR-CANDY-TABLE               OCCURS 5 TIMES.
               10  MR-CANDY-NAME            PIC X(15).
               10  MR-CANDY-BOX-SIZE        PIC X(01).
               10  MR-CANDY-TYPE            PIC X(02).
               10  MR-NBR-CASES-IN-STOCK    PIC S9(04).
               10  MR-PURCHASE-PRICE        PIC S9(03)V99.

       FD  INVENTORY-B100-FILE
           RECORD CONTAINS 143 CHARACTERS.
       01  INVENTORY-B100-RECORD            PIC X(143).    

       FD  INVENTORY-B200-FILE
           RECORD CONTAINS 143 CHARACTERS.
       01  INVENTORY-B200-RECORD            PIC X(143).    

       FD  INVENTORY-B300-FILE
           RECORD CONTAINS 143 CHARACTERS.
       01  INVENTORY-B300-RECORD            PIC X(143).    
               
       FD  INVENTORY-B400-FILE
           RECORD CONTAINS 143 CHARACTERS.
       01  INVENTORY-B400-RECORD            PIC X(143).    

       FD  EMPLOYEE-B100-FILE
           RECORD CONTAINS 85 CHARACTERS.
       01  EMPLOYEE-B100-RECORD             PIC X(85).    

       FD  EMPLOYEE-B200-FILE
           RECORD CONTAINS 85 CHARACTERS.
       01  EMPLOYEE-B200-RECORD             PIC X(85).    

       FD  EMPLOYEE-B300-FILE
           RECORD CONTAINS 85 CHARACTERS.
       01  EMPLOYEE-B300-RECORD             PIC X(85).    

       FD  EMPLOYEE-B400-FILE
           RECORD CONTAINS 85 CHARACTERS.
       01  EMPLOYEE-B400-RECORD             PIC X(85).            

       FD  ERROR-FILE
           RECORD CONTAINS 224 CHARACTERS.
       01  ERROR-RECORD                     PIC X(224).

       WORKING-STORAGE SECTION.

       01  WS-FIELDS.
           05  WS-EOF                       PIC X       VALUE 'N'.
           05  WS-WAREHOUSE-ID              PIC X(04)   VALUE SPACES.
           05  SUB                          PIC 9(02)   VALUE ZERO.

      ***********************TABLE**********************************
      * VALUE OF MESS WAREHOUSE ID AND THE NEW WAREHOUSE ID
      **************************************************************
       01  WAREHOUSE-TEXT.
           05  PIC X(08) VALUE 'BHAMB100'.
           05  PIC X(08) VALUE 'HUNTB200'.
           05  PIC X(08) VALUE 'ANNIB300'.
           05  PIC X(08) VALUE 'MONTB400'.

       01  WAREHOUSE-TABLE REDEFINES WAREHOUSE-TEXT.
           05  WAREHOUSE-ID-TABLE           OCCURS 4 TIMES.
               10  WT-WAREHOUSE-ID          PIC X(04).
               10  WT-NEW-WAREHOUSE-ID      PIC X(04).
 
      *********      OUTPUT AREA        *********
       01  INVENTORY-RECORD.
           05  IR-WAREHOUSE-ID              PIC X(04).
           05  IR-VENDOR-ID                 PIC X(01).
           05  IR-CANDY-ID                  PIC X(03).
           05  IR-CANDY-TABLE               OCCURS 5 TIMES.
               10  IR-CANDY-NAME            PIC X(15).
               10  IR-CANDY-BOX-SIZE        PIC X(01).
               10  IR-CANDY-TYPE            PIC X(02).
               10  IR-NBR-CASES-IN-STOCK    PIC 9(04).
               10  IR-PURCHASE-PRICE        PIC 9(03)V99.

       01  EMPLOYEE-RECORD.
           05  ER-WAREHOUSE-ID              PIC X(04).
           05  ER-EMPLOYEE-ID               PIC X(05).
           05  ER-EMPLOYEE-POSITION         PIC X(01).
           05  ER-EMPLOYEE-LAST-NAME        PIC X(10).
           05  ER-EMPLOYEE-FIRST-NAME       PIC X(10).
           05  ER-EMPLOYEE-MID-INITIAL      PIC X(01).
           05                               PIC X(02).
           05  ER-HIRE-DATE                 PIC 9(08).
           05                               PIC X(25).
           05  ER-CURRENT-YEARLY-SALARY     PIC 9(06)V99.
           05  ER-NUMBER-OF-DEPENDENTS      PIC 9(02).
           05  ER-HEALTH-PLAN               PIC X(01).
           05  ER-HEALTH-INS-COST           PIC 9(03).
           05                               PIC X(04).
       
      /
       PROCEDURE DIVISION.

       0010-MAIN.

           PERFORM 0015-HOUSEKEEPING
           PERFORM 0100-READ-MESS-FILE
           PERFORM 0900-EOF-ROUTINE
           .

       0015-HOUSEKEEPING.

           OPEN INPUT MESS-FILE
               OUTPUT INVENTORY-B100-FILE 
                      INVENTORY-B200-FILE 
                      INVENTORY-B300-FILE 
                      INVENTORY-B400-FILE 
                      EMPLOYEE-B100-FILE
                      EMPLOYEE-B200-FILE 
                      EMPLOYEE-B300-FILE 
                      EMPLOYEE-B400-FILE 
                      ERROR-FILE 
           .

       0100-READ-MESS-FILE.
      *************************************************************
      * READ MESS INPUT FILE
      *************************************************************

           PERFORM UNTIL WS-EOF = 'Y'
               READ MESS-FILE 
                   AT END
                       MOVE 'Y'          TO WS-EOF
                   NOT AT END
                       PERFORM 0200-PROCESS-MESS 
               END-READ
           END-PERFORM
           .

       0200-PROCESS-MESS.
      *************************************************************
      * PROCESS INCOMING MESS FILE CONVERTING WAREHOUSE ID TO THE 
      * NEW WAREHOUSE ID WHICH WILL BE USED TO DETERMINE WHICH
      * NEW FILE TO WRITE OR IF INVALID, WRITE ERROR FILE
      *************************************************************
           
           PERFORM 0220-CHECK-WAREHOUSE-ID VARYING SUB FROM 1 BY 1
              UNTIL SUB > 4
           
           EVALUATE WS-WAREHOUSE-ID
              WHEN 'B100'
                 PERFORM 0300-BUILD-INVENTORY-FILE                 
                 PERFORM 0700-WRITE-IR100-FILE
                 PERFORM 0400-BUILD-EMPLOYEE-FILE
                 PERFORM 0800-WRITE-ER100-FILE
              WHEN 'B200'
                 PERFORM 0300-BUILD-INVENTORY-FILE                 
                 PERFORM 0720-WRITE-IR200-FILE
                 PERFORM 0400-BUILD-EMPLOYEE-FILE
                 PERFORM 0820-WRITE-ER200-FILE
              WHEN 'B300'
                 PERFORM 0300-BUILD-INVENTORY-FILE                 
                 PERFORM 0730-WRITE-IR300-FILE
                 PERFORM 0400-BUILD-EMPLOYEE-FILE
                 PERFORM 0830-WRITE-ER300-FILE
              WHEN 'B400'
                 PERFORM 0300-BUILD-INVENTORY-FILE                 
                 PERFORM 0740-WRITE-IR400-FILE
                 PERFORM 0400-BUILD-EMPLOYEE-FILE
                 PERFORM 0840-WRITE-ER400-FILE    
              WHEN OTHER 
                 PERFORM 0600-WRITE-ERROR-FILE                      
           END-EVALUATE

           .

       0220-CHECK-WAREHOUSE-ID.
      *************************************************************
      * CONVERT WAREHOUSE ID TO NEW WAREHOUSE ID BASED ON DATA IN
      * WAREHOUSE-TABLE IN WORKING STORAGE
      *************************************************************

           IF MR-WAREHOUSE-ID = WT-WAREHOUSE-ID(SUB)
              MOVE WT-NEW-WAREHOUSE-ID(SUB) TO WS-WAREHOUSE-ID
              MOVE 4                        TO SUB
           ELSE 
              MOVE MR-WAREHOUSE-ID          TO WS-WAREHOUSE-ID
           END-IF

           .

       0300-BUILD-INVENTORY-FILE.
      *************************************************************
      * BUILD NEW INVENTORY FILE FROM THE ORIGINAL MESS FILE
      * 
      * WS-WAREHOUSE-ID POPULATED ABOVE BASED ON NEW WAREHOUSE ID
      * CONVERSION
      *************************************************************

           MOVE WS-WAREHOUSE-ID          TO IR-WAREHOUSE-ID
           MOVE MR-VENDOR-ID             TO IR-VENDOR-ID
           MOVE MR-CANDY-ID              TO IR-CANDY-ID

           PERFORM 0320-PROCESS-CANDY-TABLE 
              VARYING SUB FROM 1 BY 1
                 UNTIL SUB > 5
           
           .

       0320-PROCESS-CANDY-TABLE.
      *************************************************************
      * POPULATE THE INVENTORY FILE CANDY ARRAY FROM THE ORIGINAL
      * MESS FILE CANDY ARRAY
      *************************************************************
          
           MOVE MR-CANDY-NAME(SUB)      TO IR-CANDY-NAME(SUB)
           MOVE MR-CANDY-BOX-SIZE(SUB)  TO IR-CANDY-BOX-SIZE(SUB)
           MOVE MR-CANDY-TYPE(SUB)      TO IR-CANDY-TYPE(SUB)
           MOVE MR-NBR-CASES-IN-STOCK(SUB) 
                                        TO IR-NBR-CASES-IN-STOCK(SUB)
           MOVE MR-PURCHASE-PRICE(SUB)  TO IR-PURCHASE-PRICE(SUB)

           .

        0400-BUILD-EMPLOYEE-FILE.
      *************************************************************
      * BUILD NEW EMPLOYEE FILE FROM THE ORIGINAL MESS FILE
      * 
      * WS-WAREHOUSE-ID POPULATED ABOVE BASED ON NEW WAREHOUSE ID
      * CONVERSION
      *************************************************************
      
           MOVE WS-WAREHOUSE-ID          TO ER-WAREHOUSE-ID
           MOVE MR-EMPLOYEE-ID           TO ER-EMPLOYEE-ID 
           MOVE MR-EMPLOYEE-POSITION     TO ER-EMPLOYEE-POSITION
           MOVE MR-EMPLOYEE-LAST-NAME    TO ER-EMPLOYEE-LAST-NAME
           MOVE MR-EMPLOYEE-FIRST-NAME   TO ER-EMPLOYEE-FIRST-NAME
           MOVE MR-EMPLOYEE-MID-INITIAL  TO ER-EMPLOYEE-MID-INITIAL
           MOVE MR-HIRE-DATE             TO ER-HIRE-DATE
           MOVE MR-CURRENT-YEARLY-SALARY TO ER-CURRENT-YEARLY-SALARY 
           MOVE MR-NUMBER-OF-DEPENDENTS  TO ER-NUMBER-OF-DEPENDENTS
           MOVE MR-HEALTH-PLAN           TO ER-HEALTH-PLAN 
           MOVE MR-HEALTH-INS-COST       TO ER-HEALTH-INS-COST

           .

        0600-WRITE-ERROR-FILE.
      *************************************************************
      * WRITE ERROR FILE
      *************************************************************

           WRITE ERROR-RECORD FROM MESS-RECORD.
               
        0700-WRITE-IR100-FILE.
      *************************************************************
      * WRITE B100 WAREHOUSE ID VENDOR FILE
      *************************************************************

           WRITE INVENTORY-B100-RECORD FROM INVENTORY-RECORD.

        0720-WRITE-IR200-FILE.
      *************************************************************
      * WRITE B200 WAREHOUSE ID VENDOR FILE
      *************************************************************

           WRITE INVENTORY-B200-RECORD FROM INVENTORY-RECORD.
        
        0730-WRITE-IR300-FILE.
      *************************************************************
      * WRITE B300 WAREHOUSE ID VENDOR FILE
      *************************************************************

           WRITE INVENTORY-B300-RECORD FROM INVENTORY-RECORD.

        0740-WRITE-IR400-FILE.
      *************************************************************
      * WRITE B400 WAREHOUSE ID VENDOR FILE
      *************************************************************

           WRITE INVENTORY-B400-RECORD FROM INVENTORY-RECORD.

        0800-WRITE-ER100-FILE.
      *************************************************************
      * WRITE B100 WAREHOUSE ID EMPLOYEE FILE
      *************************************************************

           WRITE EMPLOYEE-B100-RECORD FROM EMPLOYEE-RECORD.

        0820-WRITE-ER200-FILE.
      *************************************************************
      * WRITE B200 WAREHOUSE ID EMPLOYEE FILE
      *************************************************************
      
           WRITE EMPLOYEE-B200-RECORD FROM EMPLOYEE-RECORD.

        0830-WRITE-ER300-FILE.
      *************************************************************
      * WRITE B300 WAREHOUSE ID EMPLOYEE FILE
      *************************************************************
      
           WRITE EMPLOYEE-B300-RECORD FROM EMPLOYEE-RECORD.

        0840-WRITE-ER400-FILE.
      *************************************************************
      * WRITE B400 WAREHOUSE ID EMPLOYEE FILE
      *************************************************************
      
           WRITE EMPLOYEE-B400-RECORD FROM EMPLOYEE-RECORD.

       0900-EOF-ROUTINE.

      * CLOSE FILES
           CLOSE MESS-FILE
                 INVENTORY-B100-FILE 
                 INVENTORY-B200-FILE 
                 INVENTORY-B300-FILE 
                 INVENTORY-B400-FILE 
                 EMPLOYEE-B100-FILE
                 EMPLOYEE-B200-FILE 
                 EMPLOYEE-B300-FILE 
                 EMPLOYEE-B400-FILE 
                 ERROR-FILE

           STOP RUN
           .


