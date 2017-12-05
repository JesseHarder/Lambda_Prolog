/* Print Controls */
should_log_between_test_types :- fail.
should_log_between_tests :- fail.
write_btt(T) :- (should_log_between_test_types -> write(T); true).
write_bt(T) :- (should_log_between_tests -> write(T); true).
