IDENTIFICATION DIVISION.
PROGRAM-ID. DATEVAL.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-DATE PIC X(8).
01 WS-YEAR PIC 9(4).
01 WS-MON PIC 99.
01 WS-DAY PIC 99.
01 WS-MAX PIC 99 VALUE 0.
PROCEDURE DIVISION.
    DISPLAY "ENTER DATE (YYYYMMDD):"
    ACCEPT WS-DATE
    IF WS-DATE IS NUMERIC
        MOVE WS-DATE(1:4) TO WS-YEAR
        MOVE WS-DATE(5:2) TO WS-MON
        MOVE WS-DATE(7:2) TO WS-DAY
        IF WS-MON < 1 OR WS-MON > 12
            DISPLAY "INVALID MONTH"
        ELSE
            EVALUATE WS-MON
                WHEN 1 WHEN 3 WHEN 5 WHEN 7 WHEN 8 WHEN 10 WHEN 12
                    MOVE 31 TO WS-MAX
                WHEN 4 WHEN 6 WHEN 9 WHEN 11
                    MOVE 30 TO WS-MAX
                WHEN 2
                    MOVE 28 TO WS-MAX
                    IF FUNCTION MOD(WS-YEAR, 400) = 0
                        MOVE 29 TO WS-MAX
                    ELSE
                        IF FUNCTION MOD(WS-YEAR, 4) = 0 AND FUNCTION MOD(WS-YEAR, 100) NOT = 0
                            MOVE 29 TO WS-MAX
                        END-IF
                    END-IF
            END-EVALUATE
            IF WS-DAY < 1 OR WS-DAY > WS-MAX
                DISPLAY "INVALID DAY"
            ELSE
                DISPLAY "ACCEPTED"
            END-IF
        END-IF
    ELSE
        DISPLAY "INVALID DATE FORMAT"
    END-IF
    STOP RUN.
