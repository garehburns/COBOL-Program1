       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLCOB.
       AUTHOR. M J PETERS.
      ****************************************************************
      * This is a sample program which produces a CLASS GRADES
      * REPORT listing student names and averages for four tests.  It
      * also calculates the overall average for the class. (S01)
      * ******
      * INPUT:
      *    The STUDENT GRADE FILE contains the following
      *    data in each record:
      *         1.  STUDENT NAME
      *         2.  THE TEST SCORES FOR EACH OF 4 EXAMS
      *
      * *******
      * OUTPUT:
      *    The GRADE REPORT contains the following information:
      *    ************
      *    DETAIL LINE:
      *         1.  STUDENT NAME
      *         2.  THE AVERAGE OF THE 4 EXAM GRADES FOR EACH STUDENT
      *    *************
      *    FINAL TOTALS:
      *         1.  AVERAGE GRADE OF THE 4 EXAM GRADES FOR ALL STUDENTS
      * *************
      * CALCULATIONS:
      *    TOTAL GRADE FOR EACH STUDENT =
      *        THE SUM OF THE FOUR EXAMS
      *    AVERAGE GRADE FOR EACH STUDENT =
      *        THE TOTAL GRADE FOR EACH STUDENT / 4 (NUMBER OF EXAMS)
      *    TOTAL OF ALL AVERAGES =
      *        THE SUM OF EACH OF THE AVERAGE GRADES
      *    NUMBER OF STUDENTS =
      *        A COUNT OF THE NUMBER OF STUDENTS IN THE CLASS
      *    AVERAGE GRADE FOR ALL STUDENTS =
      *        THE TOTAL OF ALL AVERAGES / NUMBER OF STUDENTS
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-PC.
       OBJECT-COMPUTER.  IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GRADE-FILE
               ASSIGN TO 'GRADES.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT GRADE-REPORT-FILE
               ASSIGN TO PRINTER 'PRGRADES'.

       DATA DIVISION.
       FILE SECTION.

       FD  GRADE-FILE
           RECORD CONTAINS 80 CHARACTERS.

       01  GRADE-RECORD.
           05  GR-NAME                 PIC X(20).
           05  GR-FIRST-EXAM           PIC S999.
           05  GR-SECOND-EXAM          PIC S999.
           05  GR-THIRD-EXAM           PIC S999.
           05  GR-FOURTH-EXAM          PIC S999.
           05  FILLER                  PIC X(48).

       FD  GRADE-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.

       01  REPORT-RECORD               PIC X(80).

       WORKING-STORAGE SECTION.

       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                PIC X         VALUE ' '.
               88  NO-MORE-DATA                      VALUE 'N'.

       01  DETAIL-FIELDS.
           05  DF-AVERAGE              PIC S999V9    VALUE +0.
           05  DF-SUM-GRADES           PIC S9(4)     VALUE +0.

       01  TOTAL-FIELDS.
           05  TF-NUM-STUDENTS         PIC S9(2)     VALUE +0.
           05  TF-SUM-AVERAGES         PIC S9(5)V9   VALUE +0.
           05  TF-CLASS-AVERAGE        PIC S999V9    VALUE +0.

       01  REPORT-FIELDS.
           05  PROPER-SPACING          PIC S9        VALUE +3.

       01  CONSTANTS-FIELDS.
           05  CF-NUM-TESTS            PIC S9        VALUE +4.

      **************        OUTPUT AREA        ********************

       01  HEADING-ONE.
           05                          PIC X(22)     VALUE '   Y3I'.
           05                          PIC X(22)     VALUE 'CLASS GRADES'.
           05  H1-DATE                 PIC 9999/99/99.

       01  HEADING-TWO.
           05                          PIC X(7)      VALUE SPACES.
           05                          PIC X(42)     VALUE 'NAME'.
           05                          PIC X(7)      VALUE 'AVERAGE'.

       01  DETAIL-LINE.
           05                          PIC X(5)      VALUE SPACES.
           05  DL-NAME                 PIC X(20).
           05                          PIC X(25)     VALUE SPACES.
           05  DL-AVERAGE              PIC ZZ9.9.

       01  TOTAL-LINE.
           05  FILLER                  PIC X(25)     VALUE SPACES.
           05  FILLER                  PIC X(25)     VALUE
                                        'CLASS AVERAGE'.
           05  TL-CLASS-AVERAGE        PIC ZZ9.9.
      /
       PROCEDURE DIVISION.
      *                                Y3I
       10-CONTROL-MODULE.

           PERFORM 15-HSKPING-ROUTINE 
           PERFORM 25-PROCESS-STUDENT-ROUTINE
           PERFORM 40-EOF-ROUTINE
           .
       15-HSKPING-ROUTINE.

           OPEN INPUT GRADE-FILE
               OUTPUT GRADE-REPORT-FILE 
           ACCEPT H1-DATE FROM DATE YYYYMMDD
           PERFORM 20-HEADER-ROUTINE
           .
      *  This is an example of a comment line.  It can be used
      *  throughout your program to add NECESSARY explanation.

       20-HEADER-ROUTINE.

           WRITE REPORT-RECORD FROM HEADING-ONE
               AFTER ADVANCING PAGE
           MOVE 3 TO PROPER-SPACING 
           MOVE HEADING-TWO TO REPORT-RECORD 
           PERFORM 35-WRITE-A-LINE 
           MOVE 2 TO PROPER-SPACING 
           .
       25-PROCESS-STUDENT-ROUTINE.

           PERFORM UNTIL NO-MORE-DATA
               READ GRADE-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 30-STUDENT-AVG-ROUTINE
               END-READ
           END-PERFORM
           .
       30-STUDENT-AVG-ROUTINE.

           MOVE GR-NAME TO DL-NAME
           ADD GR-FIRST-EXAM, GR-SECOND-EXAM, GR-THIRD-EXAM,
               GR-FOURTH-EXAM
                   GIVING DF-SUM-GRADES 
           DIVIDE CF-NUM-TESTS INTO DF-SUM-GRADES
               GIVING DF-AVERAGE ROUNDED 
           MOVE DF-AVERAGE TO DL-AVERAGE 

           MOVE DETAIL-LINE TO REPORT-RECORD 
           PERFORM 35-WRITE-A-LINE 
           MOVE 1 TO PROPER-SPACING 

           ADD 1 TO TF-NUM-STUDENTS 
           ADD DF-AVERAGE TO TF-SUM-AVERAGES 
           .
       35-WRITE-A-LINE.

           WRITE REPORT-RECORD
               AFTER ADVANCING PROPER-SPACING
           .
       40-EOF-ROUTINE.

           PERFORM 45-CLASS-AVERAGE-ROUTINE
           CLOSE GRADE-FILE
               GRADE-REPORT-FILE
           STOP RUN
           .
       45-CLASS-AVERAGE-ROUTINE.

           COMPUTE TF-CLASS-AVERAGE ROUNDED =
               TF-SUM-AVERAGES / TF-NUM-STUDENTS
           MOVE TF-CLASS-AVERAGE TO TL-CLASS-AVERAGE
           MOVE TOTAL-LINE TO REPORT-RECORD 
           MOVE 3 TO PROPER-SPACING 
           PERFORM 35-WRITE-A-LINE 
           .
