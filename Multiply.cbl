      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTIPLYEXAMPLE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  LandArea    PIC 9(9).
       01  Magnitude   PIC 9(9).
       01  Total       PIC 9(9).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Enter Land Area"
            ACCEPT LandArea.
            DISPLAY "Enter Magnitude"
            ACCEPT Magnitude.
            MULTIPLY LandArea BY Magnitude GIVING Total.
            DISPLAY    "Total area is ", Total.
            STOP RUN.
       END PROGRAM MULTIPLYEXAMPLE.
