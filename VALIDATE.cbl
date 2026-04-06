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
       01 INPUT-DATA-RECORD  PIC X(80).

       FD INV-DATA
           RECORDING MODE IS F.   
       01 INV-DATA-RECORD    PIC X(80).

       FD VALREPO
           RECORDING MODE IS F.
       01 VALREPO-REC     PIC X(132).

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
          05 WS-IN-ID        PIC X(5).
          05 WS-IN-SCORE1    PIC X(3).
          05 WS-IN-SCORE2    PIC X(3).
          05 WS-IN-SCORE3    PIC X(3).
          05 WS-IN-CATEGORY  PIC X.

      *********************************
      * Flags for tracking
       01 WS-FLAGS.
          05 WS-EOF          PIC X      VALUE "N".

      *********************************
      * Counters for:
      * Records read, invalid records,
      * valid records, average values, etc
       01 WS-COUNTERS.
          05 WS-REC-READ     PIC 9(05)  VALUE 0.
          05 WS-REC-VALID    PIC 9(05)  VALUE 0.
          05 WS-REC-INVALID  PIC 9(05)  VALUE 0.

      *********************************
      * Header for the valid data report
       01 WS-HEADER-LINE-1.
          05 FILLER          PIC X(5)   VALUE "ID   ".
          05 FILLER          PIC X(7)   VALUE "Score 1".


       01 WS-HEADER-LINE-2.
          05 FILLER          PIC X(80)  VALUE ALL "-".

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
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

       END PROGRAM RECVAL.
