- 👋 Hi, I’m @KFNJcourts
 - 🌱 I’m currently learning how to use how to use GitHub to extract business rules based on Cobol program


<!---
    1000-INITIALIZATION.                                                00910000
                                                                        00921000
           PERFORM 2000-START-PROCESS THRU 2000-EXIT.                   00922001
           PERFORM 9000-FINISH THRU 9000-EXIT.                          00940000
                                                                        00950000
       1000-EXIT. EXIT.                                                 00960000
                                                                        00982200
       2000-START-PROCESS.                                              00983000
                                                                        00990000
           INITIALIZE  WS-NULL-IND                                      01080009
                       DCL-CMT-MUNI-WARR-TRIG.                          01080100
                                                                        01080200
           PERFORM 2100-MOVE-SP-INPUT THRU 2100-EXIT.                   01080300
                                                                        01080400
           PERFORM 3000-CALL-SP THRU 3000-EXIT.                         01090000
                                                                        01100000
       2000-EXIT. EXIT.                                                 01110000
                                                                        01110100
       2100-MOVE-SP-INPUT.                                              01110200
                                                                        01110300
           MOVE CALL-B0045-MUNI-WARR-TRIG-KEY TO C144-MUNI-WARR-TRIG-KEY01111000
           MOVE CALL-B0045-FUNCTION-NAME      TO C144-FUNCTION-NAME     01112008
           MOVE CALL-B0045-APP-NAME           TO C144-APP-NAME          01113008
           MOVE CALL-B0045-CRT-CD             TO C144-CRT-CD            01114008
           MOVE CALL-B0045-CASE-ALPHA         TO C144-CASE-ALPHA        01115008
           MOVE CALL-B0045-CASE-YEAR          TO C144-CASE-YEAR         01116008
           MOVE CALL-B0045-CASE-SEQ-NUM       TO C144-CASE-SEQ-NUM      01117008
           MOVE CALL-B0045-CASE-TYPE-CD       TO C144-CASE-TYPE-CD      01118008
           MOVE CALL-B0045-WARRANT-ACTION     TO C144-WARRANT-ACTION    01119208
           MOVE CALL-B0045-PROCESSED-IND      TO C144-PROCESSED-IND     01119308
           MOVE CALL-B0045-COMMENTS           TO C144-COMMENTS          01119408
           MOVE CALL-B0045-CREATE-USER-ID     TO C144-CREATE-USER-ID    01119508
           MOVE CALL-B0045-MAINT-USER-ID      TO C144-MAINT-USER-ID     01119708
           MOVE CALL-B0045-WARR-TR-CALL-IND   TO WS-WARR-TR-CALL-IND.   01119908
                                                                        01120000
           IF CALL-B0045-CASE-TYPE-CD = 'C'                             01120100
              IF CALL-B0045-WARRANT-ID-KEY = 0                          01121000
                 DISPLAY 'NO WARANT KEY PROVIDED'                       01121100
                 GO TO 9000-FINISH                                      01122000
              ELSE                                                      01122200
                 MOVE CALL-B0045-WARRANT-ID-KEY TO C144-WARRANT-ID-KEY  01122300
                 MOVE CALL-B0045-WARRANT-NUMBER TO C144-WARRANT-NUMBER  01122400
              END-IF                                                    01122500
           END-IF.                                                      01123000
                                                                        01123100
           IF CALL-B0045-CASE-TYPE-CD = 'T'                             01123200
              PERFORM 2200-GET-WARRANT-INFO                             01123300
                 THRU 2200-EXIT                                         01123400
           END-IF.                                                      01123700
                                                                        01123800
       2100-EXIT. EXIT.                                                 01123900
                                                                        01124000
       2200-GET-WARRANT-INFO.                                           01125000
                                                                        01126000
           MOVE CALL-B0045-CRT-CD        TO T025-CRT-CD                 01126100
           MOVE CALL-B0045-CASE-ALPHA    TO T025-TIC-ALPHA              01126200
           MOVE CALL-B0045-CASE-SEQ-NUM  TO T025-TIC-NUMERIC.           01126400
                                                                        01126500
 ***************START SEGMENT LOGIC                                     01126601
           MOVE CALL-B0045-CRT-CD        TO WS-CRT-CD-SEG.              01126802
           EVALUATE TRUE                                                01126901
              WHEN WS-CRT-CD-SEG(1:2) = '09'                            01127002
                 EXEC SQL                                               01127101
                    SET CURRENT PACKAGESET = 'TF10ONLN'                 01127201
                 END-EXEC                                               01127301
                                                                        01127401
              WHEN WS-CRT-CD-SEG(1:2) = '07'                            01127502
                 EXEC SQL                                               01127601
                    SET CURRENT PACKAGESET = 'TF11ONLN'                 01127701
                 END-EXEC                                               01127801
                                                                        01127901
              WHEN WS-CRT-CD-SEG(1:2) =                                 01128002
                   ('02' OR '14' OR '16' OR '19' OR '20')               01128101
                 EXEC SQL                                               01128201
                    SET CURRENT PACKAGESET = 'TF12ONLN'                 01128301
                 END-EXEC                                               01128401
                                                                        01128501
              WHEN WS-CRT-CD-SEG(1:2) =                                 01128602
                   ('10' OR '11' OR '12' OR '13' OR '18' OR '21')       01128701
                 EXEC SQL                                               01128801
                    SET CURRENT PACKAGESET = 'TF13ONLN'                 01128901
                 END-EXEC                                               01129001
                                                                        01129101
              WHEN WS-CRT-CD-SEG(1:2) =                                 01129202
                   ('01' OR '03' OR '04' OR '05'                        01129301
                     OR '06' OR '08' OR '15' OR '17')                   01129401
                 EXEC SQL                                               01129501
                    SET CURRENT PACKAGESET = 'TF14ONLN'                 01129601
                 END-EXEC                                               01129701
           END-EVALUATE                                                 01129801
 ***************  END SEGMENT LOGIC                                     01129901
                                                                        01130001
           EXEC SQL                                                     01130101
             SELECT T.WARRANT_TICSTUB                                   01130201
                   ,W.WARR_NUM                                          01130301
              INTO  :T025-WARRANT-TICSTUB                               01130401
                   ,:T009-WARR-NUM                                      01130501
               FROM TFT_TICSTUB         T                               01130601
                   ,TFT_WARRANT         W                               01130701
               WHERE T.CRT_CD           = :T025-CRT-CD                  01130801
                 AND T.TIC_ALPHA        = :T025-TIC-ALPHA               01130901
                 AND T.TIC_NUMERIC      = :T025-TIC-NUMERIC             01131001
                 AND T.WARRANT_TICSTUB  > 0                             01131101
                 AND T.WARRANT_TICSTUB  = W.ID_CALC_WARRANT             01131201
           END-EXEC.                                                    01131301
                                                                        01131401
           MOVE SQLCODE          TO CALL-B0045-SQL-RETURN-CODE.         01131513
           IF SQLCODE = +0                                              01131601
              MOVE T025-WARRANT-TICSTUB TO C144-WARRANT-ID-KEY          01131701
              MOVE T009-WARR-NUM        TO C144-WARRANT-NUMBER          01131801
           ELSE                                                         01131901
              IF SQLCODE = +100                                         01132001
               DISPLAY 'NO WARANT ID KEY FOUND'                         01132101
               GO TO 9000-FINISH                                        01132200
              ELSE                                                      01132300
               DISPLAY 'ERROR ACCESING TICSTUB'                         01132400
               GO TO 9000-FINISH                                        01132500
              END-IF                                                    01132600
           END-IF.                                                      01132700
                                                                        01132800
       2200-EXIT. EXIT.                                                 01132900
                                                                        01133000
       3000-CALL-SP.                                                    01134000
                                                                        01140000
           EXEC SQL CALL :WS-CALL-SP                                    01150000
              (                                                         01160000
              :C144-MUNI-WARR-TRIG-KEY  :IND-MUNI-WAR-TRIG-KEY          01240000
            , :C144-FUNCTION-NAME       :IND-FUNCTION-NAME              01250000
            , :C144-APP-NAME            :IND-APP-NAME                   01260000
            , :C144-CRT-CD              :IND-CRT-CD                     01270000
            , :C144-CASE-ALPHA          :IND-CASE-ALPHA                 01280000
            , :C144-CASE-YEAR           :IND-CASE-YEAR                  01290000
            , :C144-CASE-SEQ-NUM        :IND-CASE-SEQ-NUM               01300000
            , :C144-CASE-TYPE-CD        :IND-CASE-TYPE-CD               01310000
            , :C144-WARRANT-ID-KEY      :IND-WARRANT-ID-KEY             01320000
            , :C144-WARRANT-NUMBER      :IND-WARRANT-NUMBER             01321000
            , :C144-WARRANT-ACTION      :IND-WARRANT-ACTION             01322000
            , :C144-PROCESSED-IND       :IND-PROCESSED-IND              01323000
            , :C144-COMMENTS            :IND-COMMENTS                   01324000
            , :C144-CREATE-USER-ID      :IND-CREATE-USER-ID             01325000
            , :C144-MAINT-USER-ID       :IND-MAINT-USER-ID              01326000
            , :WS-WARR-TR-CALL-IND      :IND-WARR-TR-CALL-IND           01328000
            , :WS-SP-STATUS             :IND-SP-STATUS                  01329105
            , :WS-SP-RUN-STATUS         :IND-SP-RUN-STATUS              01329205
            , :WS-SP-NAME               :IND-SP-NAME                    01329305
            , :WS-SQL-RETURN-CODE       :IND-SQL-RETURN-CODE            01329405
            , :WS-SP-SQL-COMMAND        :IND-SP-SQL-COMMAND             01329700
            , :WS-SP-TABLE-NAME         :IND-SP-TABLE-NAME              01329800
            , :WS-SP-DATA-CHAR          :IND-SP-DATA-CHAR               01329900
            , :WS-SP-COMMENTS           :IND-SP-COMMENTS                01330000
           )                                                            01330100
           END-EXEC.                                                    01331000
                                                                        01340000
           EVALUATE SQLCODE                                             01350000
              WHEN 0000                                                 01360000
               MOVE WS-SQL-RETURN-CODE TO CALL-B0045-SQL-RETURN-CODE    01370000
              WHEN OTHER                                                01380000
               MOVE SQLCODE            TO CALL-B0045-SQL-RETURN-CODE    01390000
                                                 WS-SQLCODE             01400000
               STRING 'ERR ACCESSING SP (' WS-CALL-SP                   01410000
                    WS-SQLCODE ')' DELIMITED BY SIZE                    01420000
                    INTO CALL-B0045-SP-COMMENTS                         01430000
               DISPLAY 'ERROR ACCESSING STORED PROC ' WS-CALL-SP        01440000
               DISPLAY 'SQLCODE = ' WS-SQLCODE                          01450000
                 GO TO 3000-EXIT                                        01510000
           END-EVALUATE.                                                01520000
                                                                        01530000
           EVALUATE WS-SQL-RETURN-CODE                                  01540000
            WHEN 0000                                                   01550000
             PERFORM 3100-GET-SP-OUTPUT THRU 3100-EXIT                  01560000
            WHEN 100                                                    01610000
             PERFORM 3100-GET-SP-OUTPUT THRU 3100-EXIT                  01620000
            WHEN OTHER                                                  01670000
             MOVE WS-SQL-RETURN-CODE   TO CALL-B0045-SQL-RETURN-CODE    01680000
                                               WS-SQLCODE               01690000
             MOVE WS-SP-COMMENTS       TO CALL-B0045-SP-COMMENTS        01700000
             DISPLAY WS-CALL-SP ' FAILED'                               01710000
             DISPLAY 'SQLCODE = ' WS-SQLCODE                            01720000
           END-EVALUATE.                                                01780000
                                                                        01790000
       3000-EXIT. EXIT.                                                 01800000
                                                                        01801000
       3100-GET-SP-OUTPUT.                                              01802000
                                                                        01803000
           MOVE C144-MUNI-WARR-TRIG-KEY  TO                             01804000
                                         CALL-B0045-MUNI-WARR-TRIG-KEY  01804100
           MOVE C144-FUNCTION-NAME       TO CALL-B0045-FUNCTION-NAME    01804200
           MOVE C144-APP-NAME            TO CALL-B0045-APP-NAME         01806000
           MOVE C144-CRT-CD              TO CALL-B0045-CRT-CD           01807000
           MOVE C144-CASE-ALPHA          TO CALL-B0045-CASE-ALPHA       01808000
           MOVE C144-CRT-CD              TO CALL-B0045-CASE-YEAR        01809000
           MOVE C144-CASE-SEQ-NUM        TO CALL-B0045-CASE-SEQ-NUM     01809100
           MOVE C144-CASE-TYPE-CD        TO CALL-B0045-CASE-TYPE-CD     01809200
           MOVE C144-WARRANT-ID-KEY      TO CALL-B0045-WARRANT-ID-KEY   01809300
           MOVE C144-WARRANT-NUMBER      TO                             01809408
                                         CALL-B0045-WARRANT-NUMBER      01809512
           MOVE C144-WARRANT-ACTION      TO CALL-B0045-WARRANT-ACTION   01809600
           MOVE C144-PROCESSED-IND       TO CALL-B0045-PROCESSED-IND    01809700
           MOVE C144-COMMENTS            TO CALL-B0045-COMMENTS         01809800
           MOVE C144-CREATE-USER-ID      TO CALL-B0045-CREATE-USER-ID   01809900
           MOVE C144-MAINT-USER-ID       TO CALL-B0045-MAINT-USER-ID    01810000
           MOVE WS-SQL-RETURN-CODE       TO CALL-B0045-SQL-RETURN-CODE  01810800
           MOVE WS-SP-COMMENTS           TO CALL-B0045-SP-COMMENTS.     01811200
                                                                        01811300
       3100-EXIT. EXIT.                                                 01812000
                                                                        01820000
       9000-FINISH.                                                     01830000
                                                                        01840000
           GOBACK.                                                      01850000
                                                                        01860000
       9000-EXIT. EXIT.                                                 01870000
                                                                        01880000
--->
