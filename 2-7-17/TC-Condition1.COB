      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RentCharge.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  CityCode     PIC 9 VALUE 8.
           88  Dublin              VALUE 1.
           88  Limerick            VALUE 2.
           88  Cork                VALUE 3.
           88  Galway              VALUE 4.
           88  Sligo               VALUE 5.
           88  Waterford           VALUE 6.
           88  UniversityCity      VALUE 1 THRU 4.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE    2   TO  CityCode.
           IF  Limerick
               DISPLAY "Hey, we're home!"
           END-IF.
           IF  UniversityCity
               PERFORM CalcRentSurcharge.
           END-IF.
           MOVE     6   TO CityCode
           SET Cork TO TRUE.
            STOP RUN.
       END PROGRAM RentCharge.
