       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECVAL.
       AUTHOR. Grant Heath.
       DATE-WRITTEN. 2026-4-3
      *********************************
      *
      *    Record Validation Program
      *
      *    This program reads fixed-length input records and validates
      *    each field based on numeric and category rules.
      *    Valid records are written to a formatted output file,
      *    while invalid records are routed to an error file with an
      *    error code indicating the reason
      *    for rejection.
      *
      *    | Code | Meaning          |
      *    |------|------------------|
      *    | 01   | Invalid ID       |
      *    | 02   | Invalid score    |
      *    | 03   | Invalid category |
      *
      *    The program also produces summary
      *    counts for total, valid, and invalid records.
      *
      *********************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      * Defines the:
      * Input file, Invalid data file, and the valid data report
       FILE-CONTROL.
           SELECT INPUT-DATA ASSIGN TO INFILE.
           SELECT INV-DATA   ASSIGN TO INVFILE.
           SELECT VALREPO  ASSIGN TO VALREPO.
       DATA DIVISION.
       FILE SECTION.

       FD INPUT-DATA
           RECORDING MODE IS F.
       01 INPUT-DATA-RECORD      PIC X(80).

       FD INV-DATA
           RECORDING MODE IS F.
       01 INV-DATA-RECORD        PIC X(80).

       FD VALREPO
           RECORDING MODE IS F.
       01 VALREPO-REC            PIC X(132).

       WORKING-STORAGE SECTION.

      *********************************
      *    Input fields
      *    | Field     | Columns | Type    |
      *    |-----------|---------|---------|
      *    | ID        | 1-5     | Numeric |
      *    | SCORE-1   | 6-8     | Numeric |
      *    | SCORE-2   | 9-11    | Numeric |
      *    | SCORE-3   | 12-14   | Numeric |
      *    | CATEGORY  | 15      | A/B/C   |
       01 WS-IN-FIELDS.
          05 WS-IN-ID            PIC X(5).
          05 WS-IN-SCORE1        PIC X(3).
          05 WS-IN-SCORE2        PIC X(3).
          05 WS-IN-SCORE3        PIC X(3).
          05 WS-IN-CATEGORY      PIC X.

      *********************************
      * Varibles to store NUMERIC verison of SCORE varibles
       01 WS-NUMERIC-SCORES.
          05 WS-SCORE1-N         PIC 9(3).
          05 WS-SCORE2-N         PIC 9(3).
          05 WS-SCORE3-N         PIC 9(3).

      *********************************
      * Flags for tracking
       01 WS-FLAGS.
          05 WS-EOF              PIC X      VALUE "N".
             88 EOF-YES                     VALUE 'Y'.
          05 WS-RECORD-ERR       PIC X      VALUE 'N'.

      *********************************
      * Error handling
       01 WS-ERROR-DATA.
          05 WS-ERROR-CODE       PIC XX     VALUE SPACES.
      
       01 WS-ERROR-CODES.
          05 ERR-ID-NONNUMERIC   PIC XX     VALUE '01'.
          05 ERR-SCORE-INVALID   PIC XX     VALUE '02'.
          05 ERR-CAT-INVALID     PIC XX     VALUE '03'.
       
      *********************************
      * Counters for:
      * Records read, invalid records,
      * valid records, average values, etc
       01 WS-COUNTERS.
          05 WS-REC-READ         PIC 9(5)   VALUE 0.
          05 WS-REC-VALID        PIC 9(5)   VALUE 0.
          05 WS-REC-INVALID      PIC 9(5)   VALUE 0.
          05 WS-SCORE1-INVALID   PIC 9(3)   VALUE 0.
          05 WS-SCORE2-INVALID   PIC 9(3)   VALUE 0.
          05 WS-SCORE3-INVALID   PIC 9(3)   VALUE 0.

      *********************************
      * Average Trackers
       01 WS-SCORE-AVERAGES.
          05 WS-SCORE1-AVG       PIC 9(3)   VALUE 0.
          05 WS-SCORE2-AVG       PIC 9(3)   VALUE 0.
          05 WS-SCORE3-AVG       PIC 9(3)   VALUE 0.
          05 WS-SCORE-TOTAL-AVG  PIC 9(3)   VALUE 0.


      *********************************
      * Header for the valid data report
       01 WS-HEADER-LINE-1.
          05 FILLER              PIC X(2)   VALUE "ID".
          05 FILLER              PIC X(4)   VALUE ALL " ".
          05 FILLER              PIC X(7)   VALUE "Score 1".
          05 FILLER              PIC X(4)   VALUE ALL " ".
          05 FILLER              PIC X(7)   VALUE "Score 2".
          05 FILLER              PIC X(4)   VALUE ALL " ".
          05 FILLER              PIC X(7)   VALUE "Score 3".


       01 WS-HEADER-LINE-2.
          05 FILLER              PIC X(80)  VALUE ALL "-".

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-LOOP UNTIL EOF-YES.

           PERFORM 1010-CLOSE

           .

       1000-INITIALIZE.
           OPEN INPUT INPUT-DATA
                OUTPUT INV-DATA VALREPO
           WRITE VALREPO-REC FROM WS-HEADER-LINE-1
           WRITE VALREPO-REC FROM WS-HEADER-LINE-2
           .

       1010-CLOSE.
           CLOSE INPUT-DATA
                 INV-DATA
                 VALREPO
           .
      * Loops until WS-EOF is equal to EOF-YES
      * Processes each record in the file
       2000-PROCESS-LOOP.
           PERFORM 2100-READ-RECORD
           IF NOT EOF-YES
              PERFORM 2200-MOVE-FIELDS
              PERFORM 4000-ERROR-CHECK
       
      *        IF WS-RECORD-ERR = 'Y'
      *            PERFORM 5000-WRITE-INVALID
      *        ELSE
      *            PERFORM 6000-CALCULATE-AVERAGE
      *            PERFORM 7000-WRITE-VALID
      *        END-IF
      *    END-IF.
           .       

      * Read each record
       2100-READ-RECORD.
           READ INPUT-DATA
           AT END
              SET EOF-YES TO TRUE
           NOT AT END
               ADD 1 TO WS-REC-READ
           END-READ.
      
       2200-MOVE-FIELDS.
           MOVE INPUT-DATA-RECORD(1:5) TO WS-IN-ID.
           MOVE INPUT-DATA-RECORD(6:3) TO WS-IN-SCORE1.
           MOVE INPUT-DATA-RECORD(9:3) TO WS-IN-SCORE2.
           MOVE INPUT-DATA-RECORD(12:3) TO WS-IN-SCORE3.
           MOVE INPUT-DATA-RECORD(15:1) TO WS-IN-CATEGORY.


      * Checks if theres any errors in each record
       4000-ERROR-CHECK.
           PERFORM 4100-CHECK-ID
           PERFORM 4200-CHECK-SCORES
           PERFORM 4300-CHECK-CATEGORY.

      * Checks if the ID is numeric
       4100-CHECK-ID.
           *> Checks if ID is numeric
           IF WS-IN-ID IS NUMERIC
              CONTINUE
           ELSE
              MOVE ERR-ID-NONNUMERIC TO WS-ERROR-CODE
              MOVE 'Y' TO WS-RECORD-ERR 
              ADD 1 TO WS-REC-INVALID
           END-IF.

      * Checks if scores are numeric and between 0-100
       4200-CHECK-SCORES.
      *    Checks if SCORE1 is Numeric, then if its between 0-100
           IF WS-IN-SCORE1 IS NUMERIC
              *> Sets Score1 as Numeric Varible
              MOVE WS-IN-SCORE1 TO WS-SCORE1-N
              *> Checks if SCORE1 is less than 0 or over 100
              IF WS-SCORE1-N < 0 OR WS-SCORE1-N > 100
                 MOVE ERR-SCORE-INVALID TO WS-ERROR-CODE
                 MOVE 'Y' TO WS-RECORD-ERR
                 ADD 1 TO WS-REC-INVALID
              END-IF
           ELSE
              MOVE ERR-SCORE-INVALID TO WS-ERROR-CODE
              MOVE 'Y' TO WS-RECORD-ERR
              ADD 1 TO WS-REC-INVALID
              ADD 1 TO WS-SCORE1-INVALID
           END-IF

      *    Checks if SCORE2 is Numeric, then if its between 0-100
           IF WS-IN-SCORE2 IS NUMERIC
              *> Sets Score2 as Numeric Varible
              MOVE WS-IN-SCORE2 TO WS-SCORE2-N
              *> Checks if SCORE2 is less than 0 or over 100
              IF WS-SCORE2-N < 0 OR WS-SCORE2-N > 100
                 MOVE ERR-SCORE-INVALID TO WS-ERROR-CODE
                 MOVE 'Y' TO WS-RECORD-ERR
                 ADD 1 TO WS-REC-INVALID
              END-IF
           ELSE
              MOVE ERR-SCORE-INVALID TO WS-ERROR-CODE
              MOVE 'Y' TO WS-RECORD-ERR
              ADD 1 TO WS-REC-INVALID
              ADD 1 TO WS-SCORE2-INVALID
           END-IF

      *    Checks if SCORE3 is Numeric, then if its between 0-100
           IF WS-IN-SCORE3 IS NUMERIC
              *> Sets Score3 as Numeric Varible
              MOVE WS-IN-SCORE3 TO WS-SCORE3-N
              *> Checks if SCORE1 is less than 0 or over 100
              IF WS-SCORE3-N < 0 OR WS-SCORE3-N > 100
                 MOVE ERR-SCORE-INVALID TO WS-ERROR-CODE
                 MOVE 'Y' TO WS-RECORD-ERR
                 ADD 1 TO WS-REC-INVALID
              END-IF
           ELSE
              MOVE ERR-SCORE-INVALID TO WS-ERROR-CODE
              MOVE 'Y' TO WS-RECORD-ERR
              ADD 1 TO WS-REC-INVALID
              ADD 1 TO WS-SCORE3-INVALID
           END-IF.

      * Checks if Category is valid, it must be either A, B, or C
      * or else its invalid
       4300-CHECK-CATEGORY.
           *> Category must be A, B, or C
           IF WS-IN-CATEGORY NOT = 'A'
              AND WS-IN-CATEGORY NOT = 'B'
              AND WS-IN-CATEGORY NOT = 'C'

              MOVE ERR-CAT-INVALID TO WS-ERROR-CODE
              MOVE 'Y' TO WS-RECORD-ERR
              ADD 1 TO WS-REC-INVALID
           END-IF.
           
       6000-CALCULATE-AVERAGE.

       END PROGRAM RECVAL.
