      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TC-Perform1.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM VARYING Idx1 EQUAL TO 3.
                   DISPLAY Idx1
            END-PERFORM.
            STOP RUN.
       END PROGRAM TC-Perform1.
