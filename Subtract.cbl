      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBTRACTEXAMPLE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  Tax         PIC 9(9).
       01  GrossPay    PIC 9(9).
       01  Total       PIC 9(9).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY"Input Gross Pay"
           ACCEPT GrossPay.
           DISPLAY "Input Tax"
           ACCEPT Tax.
           DISPLAY "Input Total Pay"
           ACCEPT Total.
           SUBTRACT Tax FROM GrossPay, Total.
           DISPLAY "Gross Pay with Tax is " ,GrossPay,
           " total Pay will be " ,Total.

            STOP RUN.
       END PROGRAM SUBTRACTEXAMPLE.
