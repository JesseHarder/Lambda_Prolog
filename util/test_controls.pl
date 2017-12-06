/* Print Controls */
should_log_between_test_types.
should_log_between_tests.
write_btt(T) :- (should_log_between_test_types -> write(T); true).
write_bt(T) :- (should_log_between_tests -> write(T); true).
