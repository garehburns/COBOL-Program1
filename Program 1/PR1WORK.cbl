       IDENTIFICATION DIVISION.
       PROGRAM-ID. CGBPR1.
       AUTHOR. GARRETT BURNS.

      *TO RUN: COBC -XO whatever.EXE --STD=MF PR1WORK.CBL
      ***********************************************
      * INPUT:
      *   The INPUT RECORD LAYOUT contains the following data for each record:
      *       1. Warehouse ID
      *       2. Employee ID
      *       3. Employee Position
      *       4. Employee Last Name
      *       5. Employee First Name
      *       6. Hire Date
      *       7. Starting Salary
      *       8. Date of Last Pay Increase
      *       9. Current Salary
      *
      * ********
      * OUTPUT:
      *   The OUTPUT REPORT contains the following information:
      *   ********
      *   DETAIL LINE:
      *       1. Report Date
      *       2. Date
      *       3. Your Initials
      *       4. Page Number
      *       5. Warehouse ID
      *       6. Employee ID
      *       7. Employee Last Name
      *       8. Starting Salary
      *       9. Date of Last Pay Increase
      *       10. Current Salary
      *       11. Total Starting Salary
      *       12. Total Current Salary
      *   FINAL TOTALS:
      *       1. Provide a detailed employee salary report on Drakea, Ltd.
      * ********
      * CALCULATIONS:
      *
      ***********************************************
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   LAPTOP-U5VKK9JE.
       OBJECT-COMPUTER.   LAPTOP-U5VKK9JE.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-RECORDS
               ASSIGN TO 'PR1FA19.TXT'.
           SELECT EMPLOYEE-REPORT
               ASSIGN TO PRINTER 'EMPLOYEEFILE'.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD  EMPLOYEE-RECORDS
           RECORD CONTAINS 70 CHARACTERS.
       
       01  INFORMATION.
           05 I-WAREHOUSE-ID       PIC X(4).
           05 I-EMPLOYEE-ID        PIC X(5).
           05 I-EMPLOYEE-POSITION  PIC X(2).
           05 I-EMPLOYEE-LASTNAME  PIC X(10).
           05 I-EMPLOYEE-FIRSTNAME PIC X(10).
           05 FILLER               PIC X(3).
           05 I-HIRE-DATE          PIC 9(8).
           05 I-STARTING-SALARY    PIC 9(8).
           05 FILLER               PIC X(4).
           05 I-LAST-PAY-INCREASE  PIC 9(8).
           05 I-CURRENT-SALARY     PIC 9(8).
       
       FD  EMPLOYEE-REPORT
           RECORD CONTAINS 70 CHARACTERS.
       
       01  RECORD-REPORT           PIC X(79).
       
       
       WORKING-STORAGE SECTION.
       
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG            PIC X           VALUE ' '.
               88 NO-MORE-DATA                     VALUE 'N'.
               
       01  DETAIL-FIELDS.
           05  DF-STARTING         PIC S9(6)V9(2)  VALUE +0.
           05  DF-CURRENT          PIC S9(6)V9(2)  VALUE +0.
       
       01  TOTAL-FIELDS.
           05  TF-STARTING-SALARY  PIC S9(6)V9(2)  VALUE +0.
           05  TF-CURRENT-SALARY   PIC S9(6)V9(2)  VALUE +0.
           05  TF-SALARY-AVERAGE   PIC S9(6)V9(2)  VALUE +0.
           
       01  REPORT-FIELDS.
           05  PROPER-SPACING          PIC S9      VALUE +3.
       
      ********    OUTPUT AREA    ********
       
       01  HEADING-ONE.
           05  H1-DATE             PIC 9999/99/99.
           05                      PIC X(5).
           05                      PIC X(21)       VALUE 'Y3I'.
           05                      PIC X(35)       VALUE 'DRAKEA, LTD'.
           05                      PIC X(8)        VALUE 'PAGE 01'.

       01  HEADING-TWO.
           05                      PIC X(35)       VALUE SPACES.
           05                      PIC X(7)       VALUE 'SALARY '.
           05                      PIC X(6)        VALUE 'REPORT'.
           05                      PIC X(31)       VALUE SPACES.

       01  HEADING-THREE.
           05                      PIC X(3).
           05                      PIC X(9)        VALUE 'WAREHOUSE'.
           05                      PIC X(3).
           05                      PIC X(8)        VALUE 'EMPLOYEE'.
           05                      PIC X(4).
           05                      PIC X(8)        VALUE 'EMPLOYEE'.
           05                      PIC X(5).
           05                      PIC X(8)        VALUE 'STARTING'.
           05                      PIC X(8).
           05                      PIC X(4)        VALUE 'LAST'.
           05                      PIC X(8).
           05                      PIC X(7)        VALUE 'CURRENT'.
           05                      PIC X(4).

       01  HEADING-FOUR.
           05                      PIC X(6).
           05                      PIC X(2)        VALUE 'ID'.
           05                      PIC X(10).
           05                      PIC X(2)        VALUE 'ID'.
           05                      PIC X(6).
           05                      PIC X(9)        VALUE 'LAST NAME'.
           05                      PIC X(6).
           05                      PIC X(6)        VALUE 'SALARY'.
           05                      PIC X(7).
           05                      PIC X(8)        VALUE 'INCREASE'.
           05                      PIC X(6).
           05                      PIC X(6)        VALUE 'SALARY'.
           05                      PIC X(5).

       01  DETAIL-LINE.
           05                      PIC X(5)        VALUE SPACES.
           05 DL-WAREHOUSE-ID      PIC X(4).
           05                      PIC X(7)        VALUE SPACES.
           05 DL-EMPLOYEE-ID       PIC X(5).
           05                      PIC X(5)        VALUE SPACES.
           05 DL-EMPLOYEE-LASTNAME PIC X(10).
           05                      PIC X(3)        VALUE SPACES.
           05 DL-STARTING-SALARY   PIC 9(6)V9(2).
           05                      PIC X(3)        VALUE SPACES.
           05 DL-LAST-PAY-INCREASE PIC 9999/99/99.
           05                      PIC X(3)        VALUE SPACES.
           05 DL-CURRENT-SALARY    PIC 9(6)V9(2).
           05                      PIC X(2)        VALUE SPACES.

       01  TOTAL-LINE.
           05                      PIC X(31).
           05                      PIC X(8)        VALUE 'TOTAL:  '.
           05                      PIC 9(6)V9(2).
           05                      PIC X(16).
           05                      PIC 9(6)V9(2).
           05                      PIC X(2).

      /
       PROCEDURE DIVISION.
      *                                Y3I
       10-CONTROL-MODULE.
           PERFORM 15-HOUSEKEEPING-ROUTINE
           PERFORM 25-EMPLOYEE-ROUTINE
           .
       
       15-HOUSEKEEPING-ROUTINE.
           OPEN INPUT EMPLOYEE-RECORDS
               OUTPUT EMPLOYEE-REPORT
           ACCEPT H1-DATE FROM DATE YYYYMMDD
           PERFORM 20-HEADER-ROUTINE
           .
       
       20-HEADER-ROUTINE.
       
           WRITE RECORD-REPORT FROM HEADING-ONE
               AFTER ADVANCING PAGE
           
           MOVE 2 TO PROPER-SPACING
           MOVE HEADING-TWO TO RECORD-REPORT
           WRITE RECORD-REPORT FROM HEADING-ONE
               AFTER ADVANCING PAGE
           
           MOVE 3 TO PROPER-SPACING
           MOVE HEADING-THREE TO RECORD-REPORT
           WRITE RECORD-REPORT FROM HEADING-ONE
               AFTER ADVANCING PAGE
           
           MOVE 1 TO PROPER-SPACING
           MOVE HEADING-FOUR TO RECORD-REPORT
           WRITE RECORD-REPORT FROM HEADING-ONE
               AFTER ADVANCING PAGE
           .
           
       25-EMPLOYEE-ROUTINE.
           PERFORM UNTIL NO-MORE-DATA
               READ EMPLOYEE-RECORDS
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 30-TOTAL-SALARY-ROUTINE
                       .
               
       30-TOTAL-SALARY-ROUTINE.
           MOVE I-CURRENT-SALARY TO DL-CURRENT-SALARY
           MOVE I-EMPLOYEE-ID TO DL-EMPLOYEE-ID
           MOVE I-EMPLOYEE-LASTNAME TO DL-EMPLOYEE-LASTNAME
           MOVE I-STARTING-SALARY TO DL-STARTING-SALARY
           MOVE I-LAST-PAY-INCREASE TO DL-LAST-PAY-INCREASE
           MOVE I-CURRENT-SALARY TO DL-CURRENT-SALARY

           ADD I-STARTING-SALARY, TF-STARTING-SALARY GIVING DF-STARTING
           ADD I-CURRENT-SALARY, TF-CURRENT-SALARY GIVING DF-CURRENT
           .
       
       35-WRITE-A-LINE.
           WRITE RECORD-REPORT
               AFTER ADVANCING PROPER-SPACING
           .
           







