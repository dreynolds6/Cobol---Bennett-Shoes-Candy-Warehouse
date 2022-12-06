# Cobol---Bennett-Shoes-Candy-Warehouse
Bennett Shoes has purchased 4 candy warehouses and needs to determine the employees and inventory for each warehouse. Write a program to process a combined employee and inventory input file provided by the candy warehouse company to Bennett Shoes along with a single inventory input file. The single employee input file contains the data for all 4 warehouses purchased as well as a single inventory input file for all 4 warehouses. In addition, each candy warehouse purchases supplies from 6 different vendors. 

First write a program to break the employee/inventory file into a file for each warehouse detailing the employee and inventory. There should be 4 employee output files and 4 inventory output files along with an error file. Use an internal table to convert to new warehouse ID’s as detailed below.

•	Old ID to New ID
•	BHAM to B100
•	HUNT to B200
•	ANNI to B300
•	MONT to B400

There will be a total of 9 output files as detailed below:
  1) Four inventory files, one for each warehouse.
  2) Four Employee files, one for each warehouse.
  3) Error file will be written, if warehouse ID is not found in the internal table above.

Populate the 4 inventory files using the candy array from the original input file provided as part of the acquisition. The inventory is populated from/to a 5 field array denoting the candy name, box size, candy type, number of cases in stock and purchase price. 

If the warehouse id is not BHAM, HUNT, ANNI or MONT, DO NOT PROCESS it.  Write the entire input record to the error file and do not include it in the new inventory files being created.

Input File:
     01  MESS-RECORD.
           05  MR-WAREHOUSE-ID              		PIC X(04).
           05  MR-EMPLOYEE-ID               		PIC X(05).
           05  MR-EMPLOYEE-POSITION         	PIC X(02).
           05  MR-EMPLOYEE-LAST-NAME        	PIC X(10).
           05  MR-EMPLOYEE-FIRST-NAME       	PIC X(10).
           05  MR-EMPLOYEE-MID-INITIAL      	PIC X(01).
           05                               				PIC X(02).
           05  MR-HIRE-DATE                 		PIC 9(08).
           05                               				PIC X(25).
           05  MR-CURRENT-YEARLY-SALARY   	PIC 9(06)V99.
           05  MR-NUMBER-OF-DEPENDENTS      	PIC 9(02).
           05  MR-HEALTH-PLAN               		PIC X(01).
           05  MR-HEALTH-INS-COST           		PIC 9(03).
           05                               				PIC X(04).
           05  MR-VENDOR-ID                 		PIC X(01).
           05  MR-CANDY-ID                  		PIC X(03).
           05  MR-CANDY-TABLE               		OCCURS 5 TIMES.
               10  MR-CANDY-NAME            		PIC X(15).
               10  MR-CANDY-BOX-SIZE        		PIC X(01).
               10  MR-CANDY-TYPE            		PIC X(02).
               10  MR-NBR-CASES-IN-STOCK    	PIC S9(04).
               10  MR-PURCHASE-PRICE        		PIC S9(03)V99.

Output Files:
     01  INVENTORY-RECORD.
           05  IR-WAREHOUSE-ID              		PIC X(04).
           05  IR-VENDOR-ID                 		PIC X(	01).
           05  IR-CANDY-ID                  		PIC X(03).
           05  IR-CANDY-TABLE       		        	OCCURS 5 TIMES.
               10  IR-CANDY-NAME            		PIC X(15).
               10  IR-CAN	DY-BOX-SIZE   	     	PIC X(01).
               10  IR-CANDY-TYPE            		PIC X(02).
               10  IR-NBR-CASES-IN-STOCK    	PIC 9(04).
               10  IR-PURCHASE-PRICE        		PIC 9(03)V99.

       01  EMPLOYEE-RECORD.
           05  ER-WAREHOUSE-ID              		PIC X(04).
           05  ER-EMPLOYEE-ID               		PIC X(05).
           05  ER-EMPLOYEE-POSITION     		PIC X(01).
           05  ER-EMPLOYEE-LAST-NAME 		PIC X(10).
           05  ER-EMPLOYEE-FIRST-NAME		PIC X(10).
           05  ER-EMPLOYEE-MID-INITIAL 		PIC X(01).
           05                               				PIC X(02).
           05  ER-HIRE-DATE                 		PIC 9(08).
           05                               				PIC X(25).
           05  ER-CURRENT-YEARLY-SALARY     	PIC 9(06)V99.
           05  ER-NUMBER-OF-DEPENDENTS      	PIC 9(02).
           05  ER-HEALTH-PLAN               		PIC X(01).
           05  ER-HEALTH-INS-COST           		PIC 9(03).
           05                               				PIC X(04).

Next step is write a report program to sort the 4 unsorted warehouse files to create 4 sorted warehouse files: B100, B200, B300 and B400 any other values are invalid warehouses and skipped. Merge the 4 sorted files into one file to be used to write a report from the merged file showing warehouse inventory with totals by warehouse, vendor and candy along with grand total for all data.

Sort Input:	All 4 unsorted files have the same file structure
•	WAREHOUSE ID
•	VENDOR ID
•	CANDY ID
•	ARRAY OF CANDY DATA

Sort Output:  Newly sorted file containing the data of all 4 unsorted input files.

Inventory report:  Produces a listing of the file contents with sub-totals by below along with grand totals including details of candy data
•	WAREHOUSE ID
•	VENDOR ID
•	CANDY ID

 Inventory report calculations:
•	Multiply the number of cases by the case purchase price for total cost of each inventory record occurs
•	Sum the number of cases in stock and calculate total cost using:
o	WAREHOUSE ID
o	VENDOR ID
o	CANDY ID

Inventory ID conversions for reporting:
•	Vendor ID: A-Atomic Sweets, B - Boston Sweets, N-Nellies Sweet Shop, T-Tiger Treats, U-Unity Candy, X-Xtra Candies
•	Candy ID: C00, C01, C02, C03, C04, C05, C06, C07, C08, C09, C10
•	Candy box size: L – Large, M – Medium, S-Small,  F – Fundraiser, X – Sample
•	Candy type: SF – Sugar Free, SU – Sugar

Multi-level control breaks:
•	Major: Warehouse ID
•	Intermediate: Vendor ID
•	Minor: Candy ID

  
