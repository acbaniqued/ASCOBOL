      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AromaSalesReport.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT SalesFile ASSIGN TO "SALES.DAT"
                 ORGANIZATION IS LINE SEQUENTIAL.

       SELECT WorkFile ASSIGN TO "SORT.TMP".

       SELECT SummaryRep ASSIGN TO "AROMASALES.RPT"
                 ORGANIZATION IS LINE SEQUENTIAL.


       SELECT SortedFile ASSIGN TO "SORTSALE.DAT"
                 ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD SalesFile.
       01  Sales-Rec.
           88 EndOfSalesFile    VALUE HIGH-VALUES.
           02  SalesFile-CustomerID             PIC X(5).
           02  SalesFile-CustomerName           PIC X(20).
           02  SalesFile-OilID.
               03  FILLER              PIC X.
                   88 EssentialOil   VALUE "E".
               03  SalesFile-OilName         PIC 99.
           02 SalesFile-UnitSize            PIC 99.
           02 SalesFile-UnitsSold            PIC 999.


       SD WorkFile.
       01 Work-Rec.
           88 EndOfWorkFile VALUE HIGH-VALUES.
           02  WorkFile-CustomerID              PIC X(5).
           02  WorkFile-CustomerName           PIC X(20).
           02  WorkFile-OilID.
               03 FILLER               PIC X.
               03 WorkFile-OilNum           PIC 99.
           02 WorkFile-UnitSize             PIC 99.
           02 WorkFile-UnitSold            PIC 999.


       FD SummaryRep.
       01 Print-Line                   PIC X(64).

       FD SortedFile.
       01 Sorted-Rec                   PIC X(33).



       WORKING-STORAGE SECTION.

       01  Oils-Table.
           02  Oil-Cost-Values.
               03 FILLER               PIC X(40)
                       VALUE "0041003200450050002910250055003900650075".
               03 FILLER               PIC X(40)
                       VALUE "0080004400500063006500550085004812500065".
               03 FILLER               PIC X(40)
                       VALUE "0060005500670072006501250085006511150105".
           02  FILLER REDEFINES Oil-Cost-VALUES.
               03 OIL-COST           PIC 99V99 OCCURS 30 TIMES.

       01  Report-Heading-Line         PIC X(44)
                   VALUE "              AROMAMORA SUMMARY SALES REPORT".

       01  Report-Heading-Underline.
           02  FILLER                  PIC X(13) VALUE SPACES.
           02  FILLER                  PIC X(32) VALUE ALL "-".

       01  Topic-Heading-Line.
           02  FILLER                  PIC BX(13) VALUE
           " CUSTOMER NAME".
           02  FILLER                  PIC X(8) VALUE SPACES.
           02  FILLER                  PIC X(10) VALUE "CUST-ID   ".
           02  FILLER                  PIC X(8) VALUE "SALES   ".
           02  FILLER                  PIC X(11) VALUE "QTY SOLD   ".
           02  FILLER                  PIC X(11) VALUE "SALES VALUE".

       01  Cust-Sales-Line.
           02  Prn-Cust-Name           PIC X(20).
           02  Prn-Cust-Id             PIC BBB9(5).
           02  Prn-Cust-Sales          PIC BBBBBZZ9.
           02  Prn-Qty-Sold            PIC BBBBBZZ,ZZ9.
           02  Prn-Sales-Value         PIC BBBB$$$,$$9.99.

       01  Total-Sales-Line.
           02  FILLER                  PIC X(33) VALUE SPACES.
           02  FILLER                  PIC X(19) VALUE
           "TOTAL SALES       :".
           02  Prn-Total-Sales         PIC BBBBBBZZ,ZZ9.


       01  Total-Qty-Sold-Line.
           02  FILLER                  PIC X(33) VALUE SPACES.
           02  FILLER                  PIC X(19) VALUE
           "TOTAL QTY SOLD    :".
           02  Prn-Total-Qty-Sold      PIC BBBBBZZZ,ZZ9.

       01  Total-Sales-Value-Line.
           02  FILLER                  PIC X(33) VALUE SPACES.
           02  FILLER                  PIC X(19) VALUE
           "TOTAL SALES VALUE :".
           02  Prn-Total-Sales-Value   PIC B$$$$,$$9.99.

       01  Cust-Totals.
           02  Cust-Sales              PIC 999.
           02  Cust-Qty-Sold           PIC 9(5).
           02  Cust-Sales-Value        PIC 9(5)V99.

       01  Final-Totals.
           02  Total-Sales             PIC 9(5)    VALUE ZEROS.
           02  Total-Qty-Sold          PIC 9(6)    VALUE ZEROS.
           02  Total-Sales-Value       PIC 9(6)V99 VALUE ZEROS.

       01  Temp-Variables.
           02  Sale-Qty-Sold           PIC 99999.
           02  Value-Of-Sale           PIC 999999V99.
           02  Prev-Cust-Id            PIC X(5).

       PROCEDURE DIVISION.
       Produce-SummaryRep.
           SORT WorkFile ON ASCENDING WorkFile-CustomerName
                INPUT PROCEDURE IS Select-EssentialOils
                OUTPUT PROCEDURE IS Print-SummaryRep.

           STOP RUN.

       Select-EssentialOils.
           OPEN INPUT SalesFile.
           READ SalesFile
               AT END SET EndOfSalesFile TO TRUE
           END-READ.

           PERFORM UNTIL EndOfSalesFile
               IF EssentialOil
                   RELEASE Work-Rec FROM Sales-Rec
               END-IF
               READ SalesFile
                   AT END SET EndOfSalesFile TO TRUE
               END-READ
           END-PERFORM.

           CLOSE SalesFile.

       Print-SummaryRep.
           OPEN OUTPUT SummaryRep.
           OPEN OUTPUT SortedFile.
           WRITE Print-Line FROM Report-Heading-Line
           AFTER ADVANCING 1 LINE.
           WRITE Print-Line FROM Report-Heading-Underline
           AFTER ADVANCING 1 LINE.
           WRITE Print-Line FROM Topic-Heading-Line
           AFTER ADVANCING 3 LINES.

           RETURN WorkFile
               AT END SET EndOfWorkFile TO TRUE
           END-RETURN.

           PERFORM Print-Customer-Lines UNTIL EndOfWorkFile


           MOVE Total-Sales TO Prn-Total-Sales.
           WRITE Print-Line FROM Total-Sales-Line
           AFTER ADVANCING 3 LINES.

           MOVE Total-Qty-Sold TO Prn-Total-Qty-Sold.
           WRITE Print-Line FROM Total-Qty-Sold-Line
           AFTER ADVANCING 2 LINES.

           MOVE Total-Sales-Value TO Prn-Total-Sales-Value.
           WRITE Print-Line FROM Total-Sales-Value-Line
           AFTER ADVANCING 2 LINES.

           CLOSE SummaryRep, SortedFile.

       Print-Customer-Lines.
           MOVE ZEROS TO Cust-Totals.
           MOVE WorkFile-CustomerID TO Prn-Cust-Id, Prev-Cust-Id.
           MOVE WorkFile-CustomerName TO Prn-Cust-Name.

           PERFORM UNTIL WorkFile-CustomerID NOT = Prev-Cust-Id
               WRITE Sorted-Rec FROM Work-Rec
               ADD 1 TO Cust-Sales, Total-Sales

               COMPUTE Sale-Qty-Sold = WorkFile-UnitSize *
               WorkFile-UnitSold
               ADD Sale-Qty-Sold TO Cust-Qty-Sold, Total-Qty-Sold

               COMPUTE Value-Of-Sale = Sale-Qty-Sold *
               Oil-Cost(WorkFile-OilNum)
               ADD Value-Of-Sale TO Cust-Sales-Value,
               Total-Sales-Value

               RETURN WorkFile
                   AT END SET EndOfWorkFile TO TRUE
               END-RETURN
           END-PERFORM.

           MOVE Cust-Sales TO Prn-Cust-Sales.
           MOVE Cust-Qty-Sold TO Prn-Qty-Sold.
           MOVE Cust-Sales-Value TO Prn-Sales-Value.

           WRITE Print-Line FROM Cust-Sales-Line AFTER ADVANCING 2
           LINES.
