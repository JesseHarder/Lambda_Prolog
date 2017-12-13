/* Print Controls */
% Add ":- fail." to the end of the following two
% predicates to disable test printing.
should_log_between_test_types.
should_log_between_tests.
write_btt(T) :- (should_log_between_test_types -> write(T); true).
write_bt(T) :- (should_log_between_tests -> write(T); true).
