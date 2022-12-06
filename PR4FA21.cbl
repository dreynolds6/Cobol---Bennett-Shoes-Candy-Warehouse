       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PR4FA21.
       AUTHOR.         Dewelena Reynolds.
      ******************************************************************
      *  SORT 4 UNSORTED WAREHOUSE FILES TO CREATE 4 SORTED WAREHOUSE 
      *  FILES: B100, B200, B300 AND B400 ANY OTHER VALUES ARE INVALID
      *  WAREHOUSES AND SKIPPED.
      *  MERGE THE 4 SORTED FILES INTO ONE FILE
      *  WRITE A REPORT FROM THE MERGED FILE SHOWING WAREHOUSE 
      *  INVENTORY WITH TOTALS BY WAREHOUSE, VENDOR AND CANDY ALONG
      *  WITH GRAND TOTAL FOR ALL DATA
      ******
      *  INPUT
      *     ALL 4 UNSORTED FILES HAVE THE SAME FILE STRUCTURE
      *        WAREHOUSE ID
      *        VENDOR ID
      *        CANDY ID
      *        ARRAY OF CANDY DATA
      ******
      *  OUTPUT
      *     SORTED FILE CONTAINING DATA OF ALL 4 UNSORTED INPUT FILES
      *     REPORT PRODUCES A LISTING OF THE FILE CONTENTS WITH 
      *     SUB-TOTALS BY BELOW ALONG WITH GRAND TOTALS INCLUDING
      *     DETAILS OF CANDY DATA
      *        WAREHOUSE ID
      *        VENDOR ID
      *        CANDY ID
      ******
      *  CALCUATIONS
      *     MULTIPLY NUMBER OF CASES BY CASE PURCHASE PRICE FOR 
      *        TOTAL COST OF EACH INVENTORY RECORD OCCURS
      *     SUM NUMBER OF CASES IN STOCK AND TOTAL COST BY 
      *        WAREHOUSE ID
      *        VENDOR ID
      *        CANDY ID
      ******************************************************************
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.

           SELECT UNSORTED-B100
               ASSIGN TO 'NEW-INV-FILE-B100.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT UNSORTED-B200
               ASSIGN TO 'NEW-INV-FILE-B200.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT UNSORTED-B300
               ASSIGN TO 'NEW-INV-FILE-B300.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT UNSORTED-B400
               ASSIGN TO 'NEW-INV-FILE-B400.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT SORTED-B100
               ASSIGN TO 'SORTED-B100.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT SORTED-B200
               ASSIGN TO 'SORTED-B200.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT SORTED-B300
               ASSIGN TO 'SORTED-B300.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT SORTED-B400
               ASSIGN TO 'SORTED-B400.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT SORT-FILE
               ASSIGN TO 'SORTINGFILE.TMP'.
      *
           SELECT MERGED-SORTED-FILE
               ASSIGN TO 'MERGED-INV-FILE.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT INVENTORY-REPORT-FILE
               ASSIGN TO PRINTER 'INVENTORY-REPORT.TXT'.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  UNSORTED-B100
           RECORD CONTAINS 143 CHARACTERS.
       01  UNSORTED-B100-RECORD.
           05  UB100-WAREHOUSE-ID          PIC X(04).
           05  UB100-VENDOR-ID             PIC X(01).
           05  UB100-CANDY-ID              PIC X(03).
           05  FILLER                      PIC X(135).  
      *
       FD  UNSORTED-B200
           RECORD CONTAINS 143 CHARACTERS.
       01  UNSORTED-B200-RECORD.
           05  UB200-WAREHOUSE-ID          PIC X(04).
           05  UB200-VENDOR-ID             PIC X(01).
           05  UB200-CANDY-ID              PIC X(03).
           05  FILLER                      PIC X(135).
      *
       FD  UNSORTED-B300
           RECORD CONTAINS 143 CHARACTERS.
       01  UNSORTED-B300-RECORD.
           05  UB300-WAREHOUSE-ID          PIC X(04).
           05  UB300-VENDOR-ID             PIC X(01).
           05  UB300-CANDY-ID              PIC X(03).
           05  FILLER                      PIC X(135).
      *
       FD  UNSORTED-B400
           RECORD CONTAINS 143 CHARACTERS.
       01  UNSORTED-B400-RECORD.
           05  UB400-WAREHOUSE-ID          PIC X(04).
           05  UB400-VENDOR-ID             PIC X(01).
           05  UB400-CANDY-ID              PIC X(03).
           05  FILLER                      PIC X(135).
      *
       FD  SORTED-B100
           RECORD CONTAINS 143 CHARACTERS.
       01  SORTED-B100-RECORD.
           05  SB100-WAREHOUSE-ID          PIC X(04).
           05  SB100-VENDOR-ID             PIC X(01).
           05  SB100-CANDY-ID              PIC X(03).
           05  FILLER                      PIC X(135).              
      *
       FD  SORTED-B200
           RECORD CONTAINS 143 CHARACTERS.
       01  SORTED-B200-RECORD.
           05  SB200-WAREHOUSE-ID          PIC X(04).
           05  SB200-VENDOR-ID             PIC X(01).
           05  SB200-CANDY-ID              PIC X(03).
           05  FILLER                      PIC X(135).              
      *
       FD  SORTED-B300
           RECORD CONTAINS 143 CHARACTERS.
       01  SORTED-B300-RECORD.
           05  SB300-WAREHOUSE-ID          PIC X(04).
           05  SB300-VENDOR-ID             PIC X(01).
           05  SB300-CANDY-ID              PIC X(03).
           05  FILLER                      PIC X(135).              
      *
       FD  SORTED-B400
           RECORD CONTAINS 143 CHARACTERS.
       01  SORTED-B400-RECORD.
           05  SB400-WAREHOUSE-ID          PIC X(04).
           05  SB400-VENDOR-ID             PIC X(01).
           05  SB400-CANDY-ID              PIC X(03).
           05  FILLER                      PIC X(135).              
      *
       FD  MERGED-SORTED-FILE
           RECORD CONTAINS 143 CHARACTERS.
       01  MERGED-SORTED-REC.
           05  MS-WAREHOUSE-ID             PIC X(04).
           05  MS-VENDOR-ID                PIC X(01).
           05  MS-CANDY-ID                 PIC X(03).
           05  MS-CANDY-DATA               OCCURS 5 TIMES.
               10  MS-CANDY-NAME           PIC X(15).
               10  MS-CANDY-BOX-SIZE       PIC A(01).
               10  MS-CANDY-TYPE           PIC X(02).
               10  MS-NUM-OF-CASES         PIC 9(04).
               10  MS-CASE-PRICE           PIC 9(03)V99.             
      *
       SD  SORT-FILE
           RECORD CONTAINS 143 CHARACTERS.
       01  SORTED-RECORD.
           05  SF-WAREHOUSE-ID             PIC X(04).
           05  SF-VENDOR-ID                PIC X(01).
           05  SF-CANDY-ID                 PIC X(03).
           05  FILLER                      PIC X(135). 
      *
       FD  INVENTORY-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.
       01  REPORT-LINE                     PIC X(80).
      *
       WORKING-STORAGE SECTION.
      *
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                    PIC X        VALUE 'Y'.
               88 NO-MORE-DATA                          VALUE 'N'.
               88 MORE-RECORDS                          VALUE 'Y'.
           05  WS-FIRST-REC                PIC X        VALUE 'Y'.
      *
       01 SUBSCRIPTS.
           05  SUB                         PIC 9        VALUE ZERO.
           05  SUB-MAX                     PIC 9        VALUE 5.
      *
       01  REPORT-FIELDS.
           05  PROPER-SPACING              PIC S9       VALUE +1.
      *
       01  WS-FIELDS.
           05  WS-PREV-WAREHOUSE-ID        PIC X(04)     VALUE SPACES.
           05  WS-PREV-VENDOR-ID           PIC X(01)     VALUE SPACES.
           05  WS-PREV-VENDOR-NAME         PIC X(15)     VALUE SPACES.
           05  WS-PREV-CANDY-ID            PIC X(03)     VALUE SPACES.
           05  WS-PREV-CANDY-NAME          PIC X(15)     VALUE SPACES.
           05  WS-TOTAL-NUM-OF-CASES       PIC S9(09)    VALUE +0.
           05  WS-TOTAL-COST               PIC S9(09)V99 VALUE +0.
           05  WS-WAREHOUSE-TOTAL-NUM-CASES PIC S9(09)   VALUE +0.
           05  WS-WAREHOUSE-TOTAL-COST     PIC S9(09)V99 VALUE +0.
           05  WS-VENDOR-TOTAL-NUM-CASES   PIC S9(09)    VALUE +0.
           05  WS-VENDOR-TOTAL-COST        PIC S9(09)V99 VALUE +0.
           05  WS-CANDY-TOTAL-NUM-CASES    PIC S9(09)    VALUE +0.
           05  WS-CANDY-TOTAL-COST         PIC S9(09)V99 VALUE +0.
           05  WS-GRAND-TOTAL-NUM-CASES    PIC S9(09)    VALUE +0.
           05  WS-GRAND-TOTAL-COST         PIC S9(09)V99 VALUE +0.
      *
       01  WS-CURRENT-DATE.
           05  WS-YEAR                     PIC 9999.
           05  WS-MONTH                    PIC 99.
           05  WS-DAY                      PIC 99.

      **********************OUTPUT AREA**************************
       01  HEADING-ONE.
           05                              PIC X(28)    VALUE SPACES.
           05                              PIC X(23)    VALUE
               'BENNETT SWEETS AND MORE'.
           05                              PIC X(29)    VALUE SPACES.
      *
       01  HEADING-TWO.
           05                              PIC X(07)    VALUE SPACES.
           05  H1-DATE.
               10  H1-MONTH                PIC 99.
               10                          PIC X        VALUE '/'.
               10  H1-DAY                  PIC 99.
               10                          PIC X        VALUE '/'.
               10  H1-YEAR                 PIC 9(04).
           05                              PIC X(15)    VALUE SPACES.
           05                              PIC X(16)    VALUE
               'INVENTORY REPORT'.
           05                              PIC X(14)    VALUE SPACES.
           05                              PIC X(03)    VALUE 'DWR'.
      *
       01  HEADING-THREE.
           05                              PIC X(14)   VALUE 
               '  WAREHOUSE: '.
           05  H3-WAREHOUSE-ID             PIC X(04)   VALUE SPACES.
           05                              PIC X(62)   VALUE SPACES.
      *
       01  HEADING-FOUR.
           05                              PIC X(14)   VALUE 
               '     VENDOR: '.
           05  H4-VENDOR-NAME              PIC X(18) VALUE SPACES.
           05                              PIC X(48) VALUE SPACES.
      *
       01  HEADING-FIVE.
           05                              PIC X(14)   VALUE 
               '      CANDY: '.
           05  H5-CANDY-ID                 PIC X(03) VALUE SPACES.
           05                              PIC X(63) VALUE SPACES.
      *
       01  HEADING-SIX.
           05                              PIC X(06) VALUE SPACES.
           05                              PIC X(10) VALUE 
               'CANDY NAME'.
           05                              PIC X(08) VALUE SPACES.
           05                              PIC X(04) VALUE 'SIZE'.
           05                              PIC X(08) VALUE SPACES.
           05                              PIC X(04) VALUE 'TYPE'.
           05                              PIC X(03) VALUE SPACES.
           05                              PIC X(08) VALUE 'IN STOCK'.
           05                              PIC X(06) VALUE SPACES.
           05                              PIC X(10) VALUE 'TOTAL COST'.
           05                              PIC X(29) VALUE SPACES.
      *
       01  DETAIL-LINE.
           05                              PIC X(03) VALUE SPACES.
           05  DL-CANDY-NAME               PIC X(15).
           05                              PIC X(04) VALUE SPACES.
           05  DL-CANDY-BOX-SIZE           PIC A(10).
           05                              PIC X(05) VALUE SPACES.
           05  DL-CANDY-TYPE               PIC X(02).
           05                              PIC X(05) VALUE SPACES.
           05  DL-NUM-OF-CASES             PIC Z,ZZ9.
           05                              PIC X(07) VALUE SPACES.
           05  DL-TOTAL-COST               PIC $$$$,$$$.99.
      *
       01  TOTAL-CANDY-LINE.
           05                              PIC X(22) VALUE 
             '        TOTAL CANDY:  '.
           05  TC-CANDY-NAME               PIC X(15).
           05                              PIC X(06) VALUE SPACES.
           05  TC-NUM-OF-CASES             PIC ZZ,ZZ9.
           05                              PIC X(05) VALUE SPACES.
           05  TC-TOTAL-COST               PIC $$,$$$,$$$.99.
      *
       01  TOTAL-VENDOR-LINE.
           05                              PIC X(22) VALUE 
             '   TOTAL FOR VENDOR:  '.
           05  TV-VENDOR-NAME              PIC X(18).
           05                              PIC X(02) VALUE SPACES.
           05  TV-NUM-OF-CASES             PIC ZZZ,ZZ9.
           05                              PIC X(04) VALUE SPACES.
           05  TV-TOTAL-COST               PIC $$$,$$$,$$$.99.       
      *
       01  TOTAL-WAREHOUSE-LINE.
           05                              PIC X(22) VALUE 
             'TOTAL FOR WAREHOUSE:  '.
           05  TW-WAREHOUSE-ID             PIC X(04).
           05                              PIC X(14) VALUE SPACES.
           05  TW-NUM-OF-CASES             PIC Z,ZZZ,ZZ9.
           05                              PIC X(04) VALUE SPACES.
           05  TW-TOTAL-COST               PIC $$$,$$$,$$$.99.     
      *
       01  GRAND-TOTAL-LINE.
           05                              PIC X(25) VALUE SPACES.
           05                              PIC X(14) VALUE 
             'GRAND TOTAL:  '.
           05  GT-NUM-OF-CASES             PIC ZZ,ZZZ,ZZ9.
           05                              PIC X(01) VALUE SPACES.
           05  GT-TOTAL-COST               PIC $$,$$$,$$$,$$$.99.   
      *
       PROCEDURE DIVISION.
      *
       0100-PRINT-INVENTORY-REPORT.

           PERFORM 0150-SORT-MERGE-INV-FILES
           PERFORM 0200-HSKPING-ROUTINE
           PERFORM 0300-READ-INV-FILE
           PERFORM 9000-FINAL-ROUTINE
       .

      ********************************************************
      * SORT AND MERGE ROUTINES FOR THE 4 WAREHOUSE FILES
      ********************************************************
       0150-SORT-MERGE-INV-FILES.

           SORT SORT-FILE                
	          ON ASCENDING KEY SF-WAREHOUSE-ID 
              ON ASCENDING KEY SF-VENDOR-ID
              ON ASCENDING KEY SF-CANDY-ID
              USING UNSORTED-B100                
              GIVING SORTED-B100

           SORT SORT-FILE                
	          ON ASCENDING KEY SF-WAREHOUSE-ID 
              ON ASCENDING KEY SF-VENDOR-ID
              ON ASCENDING KEY SF-CANDY-ID
              USING UNSORTED-B200                
              GIVING SORTED-B200

           SORT SORT-FILE                
	          ON ASCENDING KEY SF-WAREHOUSE-ID 
              ON ASCENDING KEY SF-VENDOR-ID
              ON ASCENDING KEY SF-CANDY-ID
              USING UNSORTED-B300                
              GIVING SORTED-B300

           SORT SORT-FILE                
	          ON ASCENDING KEY SF-WAREHOUSE-ID 
              ON ASCENDING KEY SF-VENDOR-ID
              ON ASCENDING KEY SF-CANDY-ID
              USING UNSORTED-B400                
              GIVING SORTED-B400

           MERGE SORT-FILE 
	          ON ASCENDING KEY SF-WAREHOUSE-ID 
              ON ASCENDING KEY SF-VENDOR-ID
              ON ASCENDING KEY SF-CANDY-ID
              USING SORTED-B100, 
                    SORTED-B200,
                    SORTED-B300,
                    SORTED-B400 
              GIVING MERGED-SORTED-FILE
        .

      ********************************************************
      * OPENS THE INPUT AND OUTPUT FILES
      ********************************************************
       0200-HSKPING-ROUTINE.

           OPEN INPUT MERGED-SORTED-FILE
               OUTPUT INVENTORY-REPORT-FILE

           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD 
           MOVE WS-MONTH                 TO H1-MONTH
           MOVE WS-DAY                   TO H1-DAY
           MOVE WS-YEAR                  TO H1-YEAR

       .

      ********************************************************
      * READS THE MERGED INPUT FILE AND DOES NOT PROCESS ANY
      * RECORD WITH INVALID WAREHOUSE ID.
      ********************************************************
       0300-READ-INV-FILE.

           PERFORM UNTIL NO-MORE-DATA
              READ MERGED-SORTED-FILE
                 AT END
                    MOVE 'N'             TO EOF-FLAG
                 NOT AT END
                    IF MS-WAREHOUSE-ID = 'B100' OR 'B200' 
                                      OR 'B300' OR 'B400'
                       PERFORM 0400-PROCESS-INV-RECORD
                    END-IF 
              END-READ
           END-PERFORM

       .

      ********************************************************
      * BUILD INVENTORY REPORT DETAIL LINE AND PROCESS EACH 
      * LEVEL OF THE CONTROL BREAKS
      ********************************************************
       0400-PROCESS-INV-RECORD.

           EVALUATE TRUE 
              WHEN WS-FIRST-REC = 'Y'
                 MOVE 'N'                TO WS-FIRST-REC
                 PERFORM 7100-BUILD-WAREHOUSE-LINE
              WHEN WS-PREV-WAREHOUSE-ID NOT = MS-WAREHOUSE-ID
                 PERFORM 7350-WRITE-CANDY-TOTAL
                 PERFORM 7250-WRITE-VENDOR-TOTAL
                 PERFORM 7150-WRITE-WAREHOUSE-TOTAL
                 MOVE 3                  TO PROPER-SPACING
                 PERFORM 7100-BUILD-WAREHOUSE-LINE
              WHEN WS-PREV-VENDOR-ID NOT = MS-VENDOR-ID
                 PERFORM 7350-WRITE-CANDY-TOTAL
                 PERFORM 7250-WRITE-VENDOR-TOTAL
                 MOVE 3                  TO PROPER-SPACING
                 PERFORM 7200-BUILD-VENDOR-LINE
              WHEN WS-PREV-CANDY-ID NOT = MS-CANDY-ID 
                 PERFORM 7350-WRITE-CANDY-TOTAL
                 MOVE 3                  TO PROPER-SPACING
                 PERFORM 7300-BUILD-CANDY-LINE
           END-EVALUATE 
           
           PERFORM 1000-PROCESS-DETAIL
              VARYING SUB FROM 1 BY 1
                  UNTIL SUB > SUB-MAX
              
        .

      ********************************************************
      * PROCESS DETAIL LINES OCCURS TO DETERMINE IF THE OCCURS 
      * IS POPULATED AS TO WHETHER TO PRINT THE REPORT LINE
      ********************************************************
       1000-PROCESS-DETAIL.

      *     IF MS-CANDY-DATA(SUB) > ' '
              PERFORM 1100-PROCESS-CANDY-DATA 
      *     ELSE
      *        MOVE 6                    TO SUB
      *     END-IF

       .

      ********************************************************
      * POPULATE DETAIL LINES OF THE REPORT WITH EXPANDED DATA
      * ALONG WITH CALCULATING THE TOTAL COST IF VALID NUMERIC
      * DATA
      ********************************************************
       1100-PROCESS-CANDY-DATA.

           IF MS-CANDY-NAME(SUB) = WS-PREV-CANDY-NAME
              MOVE SPACES                TO DL-CANDY-NAME
           ELSE
              MOVE MS-CANDY-NAME(SUB)    TO DL-CANDY-NAME                      
                                            WS-PREV-CANDY-NAME
           END-IF

           EVALUATE MS-CANDY-BOX-SIZE(SUB) 
              WHEN 'S'
                 MOVE 'SMALL'            TO DL-CANDY-BOX-SIZE
              WHEN 'M'
                 MOVE 'MEDIUM'           TO DL-CANDY-BOX-SIZE
              WHEN 'L'
                 MOVE 'LARGE'            TO DL-CANDY-BOX-SIZE
              WHEN 'F'
                 MOVE 'FUNDRAISER'       TO DL-CANDY-BOX-SIZE
              WHEN 'X'
                 MOVE 'SAMPLE'           TO DL-CANDY-BOX-SIZE
              WHEN SPACES 
                 MOVE 'NO DATA'          TO DL-CANDY-BOX-SIZE
              WHEN OTHER
                 STRING 'BAD-' DELIMITED BY SIZE 
                    MS-CANDY-BOX-SIZE(SUB) DELIMITED BY SIZE
                       INTO DL-CANDY-BOX-SIZE
           END-EVALUATE 

           IF MS-CANDY-TYPE(SUB) = 'SU' OR 'SF' 
              MOVE MS-CANDY-TYPE(SUB)    TO DL-CANDY-TYPE
           ELSE
              MOVE '**'                  TO DL-CANDY-TYPE
           END-IF

           IF MS-NUM-OF-CASES(SUB) NUMERIC
              MOVE MS-NUM-OF-CASES(SUB)  TO DL-NUM-OF-CASES
              ADD MS-NUM-OF-CASES(SUB)   TO WS-TOTAL-NUM-OF-CASES 
                                            WS-CANDY-TOTAL-NUM-CASES
                                            WS-VENDOR-TOTAL-NUM-CASES 
                                            WS-WAREHOUSE-TOTAL-NUM-CASES
                                            WS-GRAND-TOTAL-NUM-CASES  
           ELSE
              MOVE ZERO                  TO DL-NUM-OF-CASES 
           END-IF
              
           IF MS-NUM-OF-CASES(SUB) NUMERIC AND 
              MS-CASE-PRICE(SUB)   NUMERIC
              COMPUTE WS-TOTAL-COST = 
                 MS-NUM-OF-CASES(SUB) * MS-CASE-PRICE(SUB) 
           ELSE
              MOVE ZERO                  TO WS-TOTAL-COST
           END-IF 
              
           MOVE WS-TOTAL-COST            TO DL-TOTAL-COST
           ADD WS-TOTAL-COST             TO WS-CANDY-TOTAL-COST
                                            WS-VENDOR-TOTAL-COST 
                                            WS-WAREHOUSE-TOTAL-COST 
                                            WS-GRAND-TOTAL-COST 
           MOVE DETAIL-LINE              TO REPORT-LINE
           PERFORM  8000-WRITE-A-LINE
              
           INITIALIZE DETAIL-LINE 
           MOVE 1                        TO PROPER-SPACING

       .

      ********************************************************
      * PRINT STATIC HEADINGS FOR INVENTORY REPORT 
      ********************************************************
       7000-HEADING-ROUTINE.

           WRITE REPORT-LINE FROM HEADING-ONE
              AFTER ADVANCING PAGE
           MOVE 2                        TO PROPER-SPACING

           WRITE REPORT-LINE FROM HEADING-TWO 
              AFTER ADVANCING PROPER-SPACING 
       .

      ********************************************************
      * BUILD/WRITE WAREHOUSE HEADER-THREE
      ********************************************************
       7100-BUILD-WAREHOUSE-LINE.
          
           PERFORM 7000-HEADING-ROUTINE 
           MOVE MS-WAREHOUSE-ID          TO H3-WAREHOUSE-ID
                                            WS-PREV-WAREHOUSE-ID 
           MOVE HEADING-THREE            TO REPORT-LINE
           PERFORM 8000-WRITE-A-LINE
           PERFORM 7200-BUILD-VENDOR-LINE
           MOVE 2                        TO PROPER-SPACING
                 
       .      

      ********************************************************
      * BUILD/WRITE THE WAREHOUSE TOTAL LINE FOR REPORT
      ********************************************************
       7150-WRITE-WAREHOUSE-TOTAL.
          
           MOVE WS-PREV-WAREHOUSE-ID         TO TW-WAREHOUSE-ID 
           MOVE WS-WAREHOUSE-TOTAL-NUM-CASES TO TW-NUM-OF-CASES
           MOVE WS-WAREHOUSE-TOTAL-COST      TO TW-TOTAL-COST 
           MOVE TOTAL-WAREHOUSE-LINE         TO REPORT-LINE
           PERFORM 8000-WRITE-A-LINE

           INITIALIZE TOTAL-WAREHOUSE-LINE 
           MOVE ZERO                     TO WS-WAREHOUSE-TOTAL-NUM-CASES
                                            WS-WAREHOUSE-TOTAL-COST

       .        

      ********************************************************
      * BUILD/WRITE VENDOR HEADING-FOUR WITH THE EXPANDED DATA
      * AND PROCESS THE ADDITIONAL BREAKS
      ********************************************************
       7200-BUILD-VENDOR-LINE.
          
           MOVE MS-VENDOR-ID               TO WS-PREV-VENDOR-ID

           EVALUATE MS-VENDOR-ID
              WHEN 'A' 
                 MOVE 'ATOMIC SWEETS'      TO H4-VENDOR-NAME 
              WHEN 'B'
                 MOVE 'BOSTON SWEETS'      TO H4-VENDOR-NAME 
              WHEN 'N'
                 MOVE 'NELLIES SWEET SHOP' TO H4-VENDOR-NAME 
              WHEN 'T'
                 MOVE 'TIGER TREATS'       TO H4-VENDOR-NAME 
              WHEN 'U'
                 MOVE 'UNITY CANDY'        TO H4-VENDOR-NAME 
              WHEN 'X'
                 MOVE 'XTRA CANDIES'       TO H4-VENDOR-NAME 
              WHEN OTHER
                 MOVE 'INVALID VEND CODE'  TO H4-VENDOR-NAME 
           END-EVALUATE 
           
           MOVE H4-VENDOR-NAME             TO WS-PREV-VENDOR-NAME
           MOVE HEADING-FOUR               TO REPORT-LINE
           PERFORM 8000-WRITE-A-LINE

           MOVE 2                          TO PROPER-SPACING
           PERFORM 7300-BUILD-CANDY-LINE

       . 

      ********************************************************
      * BUILD/WRITE VENDOR TOTAL LINE FOR REPORT
      ********************************************************
       7250-WRITE-VENDOR-TOTAL.
       
           MOVE WS-PREV-VENDOR-NAME       TO TV-VENDOR-NAME
           MOVE WS-VENDOR-TOTAL-NUM-CASES TO TV-NUM-OF-CASES
           MOVE WS-VENDOR-TOTAL-COST      TO TV-TOTAL-COST 

           MOVE TOTAL-VENDOR-LINE         TO REPORT-LINE
           PERFORM 8000-WRITE-A-LINE

           INITIALIZE TOTAL-VENDOR-LINE 
           MOVE ZERO                      TO WS-VENDOR-TOTAL-NUM-CASES
                                             WS-VENDOR-TOTAL-COST

       .        

      ********************************************************
      * BUILD/WRITE CANDY HEADING-FIVE AND HEADING-SIX FOR REPORT
      ********************************************************
       7300-BUILD-CANDY-LINE.

           MOVE MS-CANDY-ID              TO H5-CANDY-ID
                                            WS-PREV-CANDY-ID 

           MOVE HEADING-FIVE             TO REPORT-LINE
           PERFORM 8000-WRITE-A-LINE

           MOVE 2                        TO PROPER-SPACING
           MOVE HEADING-SIX              TO REPORT-LINE
           PERFORM 8000-WRITE-A-LINE

       . 

      ********************************************************
      * BUILD/WRITE CANDY TOTAL LINE FOR REPORT
      ********************************************************
       7350-WRITE-CANDY-TOTAL.
      *    
           MOVE 2                        TO PROPER-SPACING
           MOVE WS-CANDY-TOTAL-NUM-CASES TO TC-NUM-OF-CASES
           MOVE WS-CANDY-TOTAL-COST      TO TC-TOTAL-COST 
           MOVE TOTAL-CANDY-LINE         TO REPORT-LINE
           PERFORM 8000-WRITE-A-LINE
           
           INITIALIZE TOTAL-CANDY-LINE 
           MOVE ZERO                     TO WS-CANDY-TOTAL-NUM-CASES
                                            WS-CANDY-TOTAL-COST
      
       .        

      ********************************************************
      * WRITE THE REPORT-LINE BUILT
      ********************************************************
       8000-WRITE-A-LINE.
       
           WRITE REPORT-LINE
               AFTER ADVANCING PROPER-SPACING
       .

      ********************************************************
      * BUILD/WRITE THE FINAL TOTAL LINES FOR REPORT INCLUDING
      * GRAND TOTAL LINE AND CLOSE FILES
      ********************************************************
       9000-FINAL-ROUTINE.
           
           PERFORM 7350-WRITE-CANDY-TOTAL
           PERFORM 7250-WRITE-VENDOR-TOTAL
           PERFORM 7150-WRITE-WAREHOUSE-TOTAL

           MOVE 3                        TO PROPER-SPACING
           MOVE WS-GRAND-TOTAL-NUM-CASES TO GT-NUM-OF-CASES
           MOVE WS-GRAND-TOTAL-COST      TO GT-TOTAL-COST 
           MOVE GRAND-TOTAL-LINE         TO REPORT-LINE
           PERFORM 8000-WRITE-A-LINE

           CLOSE MERGED-SORTED-FILE
                 INVENTORY-REPORT-FILE

           STOP RUN

       .
