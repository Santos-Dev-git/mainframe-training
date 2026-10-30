
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATOMIC-TRANSFER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 SOURCE-BALANCE       PIC 9(7)V99.
       01 DEST-BALANCE         PIC 9(7)V99.
       01 ORIGINAL-SOURCE      PIC 9(7)V99.
       01 ORIGINAL-DEST        PIC 9(7)V99.
       01 AMOUNT               PIC 9(7)V99.

       01 FAILURE-FLAG         PIC X.
       01 FAILURE-STAGE        PIC 9.
       01 TX-STATUS            PIC X(12).

       PROCEDURE DIVISION.

       MAIN-PARA.

           DISPLAY "ENTER SOURCE BALANCE:"
           ACCEPT SOURCE-BALANCE

           DISPLAY "ENTER DESTINATION BALANCE:"
           ACCEPT DEST-BALANCE

           DISPLAY "ENTER TRANSFER AMOUNT:"
           ACCEPT AMOUNT

           DISPLAY "ENTER FAILURE FLAG (Y/N):"
           ACCEPT FAILURE-FLAG

           DISPLAY "ENTER FAILURE STAGE (1-4):"
           ACCEPT FAILURE-STAGE

       *>    SAVE ORIGINAL BALANCES

           MOVE SOURCE-BALANCE TO ORIGINAL-SOURCE
           MOVE DEST-BALANCE TO ORIGINAL-DEST

       *>    CHECK TRANSFER AMOUNT

           IF AMOUNT <= 0
               DISPLAY "INVALID TRANSFER AMOUNT"
               STOP RUN
           END-IF

           IF SOURCE-BALANCE < AMOUNT
               DISPLAY "INSUFFICIENT BALANCE"
               STOP RUN
           END-IF

       *>    FAILURE BEFORE DEBIT

           IF FAILURE-FLAG = 'Y'
               AND FAILURE-STAGE = 1

               PERFORM ROLLBACK-TRANSACTION

           ELSE

       *>        DEBIT SOURCE ACCOUNT

               SUBTRACT AMOUNT FROM SOURCE-BALANCE

       *>        FAILURE BETWEEN DEBIT AND CREDIT

               IF FAILURE-FLAG = 'Y'
                   AND FAILURE-STAGE = 2

                   PERFORM ROLLBACK-TRANSACTION

               ELSE

       *>            CREDIT DESTINATION ACCOUNT

                   ADD AMOUNT TO DEST-BALANCE

       *>            FAILURE AFTER BOTH UPDATES

                   IF FAILURE-FLAG = 'Y'
                       AND FAILURE-STAGE = 3

                       PERFORM ROLLBACK-TRANSACTION

                   ELSE

                       MOVE "COMMITTED" TO TX-STATUS

                   END-IF

               END-IF

           END-IF

           DISPLAY "-------------------------"
           DISPLAY "TRANSACTION STATUS: " TX-STATUS
           DISPLAY "SOURCE BALANCE: " SOURCE-BALANCE
           DISPLAY "DESTINATION BALANCE: " DEST-BALANCE
           DISPLAY "-------------------------"

           STOP RUN.

       ROLLBACK-TRANSACTION.

           MOVE ORIGINAL-SOURCE TO SOURCE-BALANCE
           MOVE ORIGINAL-DEST TO DEST-BALANCE

           MOVE "ROLLED BACK" TO TX-STATUS.
