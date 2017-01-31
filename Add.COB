      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDINGEXAMPLES.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  Cash    PIC 9(9).
       01  Cheque  PIC 9(9).
       01  Total   PIC 9(9).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "Enter Cash"
           ACCEPT Cash.
           DISPLAY "Enter amount of Cheque"
           ACCEPT Cheque.
           ADD Cash   TO Total.
           ADD Cash, 20 TO Total.
           ADD Cash, Cheque GIVING Total.
           DISPLAY "Total Cash is ", Total, " with a Cheque of ", Cheque.
           STOP RUN.

       END PROGRAM ADDINGEXAMPLES.
