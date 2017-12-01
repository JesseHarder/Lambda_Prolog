:- [types, util/plists].
/* Print Controls */
should_log_between_test_types :- fail.
should_log_between_tests :- fail.
write_btt(T) :- (should_log_between_test_types -> write(T); true).
write_bt(T) :- (should_log_between_tests -> write(T); true).

/* ----- Unit Tests ----- */
utest_t :-  typeof(unit, 'Unit'),
	write_bt("utest_t passed.\n"),!.

all_unit_tests_pass :-
	write_btt("--- Checking Unit Type Tests. ---\n"),
	utest_t,
	write_btt("--- All Unit Type Tests Pass. ---\n"),!.
/* ----- End Unit Tests ----- */
/* ----- Bool Tests ----- */
btest1_t :- typeof(tru, 'Bool'),
	write_bt("btest1_t passed.\n"),!.
btest2_t :- typeof(fls, 'Bool'),
	write_bt("btest2_t passed.\n"),!.
btest3_t :- typeof(ifte(tru, fls, fls), 'Bool'),
	write_bt("btest3_t passed.\n"),!.
btest4_t :- typeof(  ifte(ifte(tru,fls,fls),
						ifte(fls, fls, tru),
						ifte(tru, tru, ifte(tru,tru,tru))), 'Bool'),
						write_bt("btest4_t passed.\n"),!.

all_bool_type_tests_pass :-
	write_btt("--- Checking Boolean Type Tests. ---\n"),
	btest1_t, btest2_t, btest3_t, btest4_t,
	write_btt("--- All Boolean Type Tests Pass. ---\n"),!.
/* ----- End Bool Tests ----- */
/* ----- Natural Tests ----- */
ntest1_t :- typeof(0, 'Natural'),
	write_bt("ntest1_t passed.\n"),!.
ntest2_t :- typeof(succ(0), 'Natural'),
	write_bt("ntest2_t passed.\n"),!.
ntest3_t :- typeof(pred(succ(0)), 'Natural'),
	write_bt("ntest3_t passed.\n"),!.
ntest4_t :- typeof(iszero(0), 'Bool'),
	write_bt("ntest4_t passed.\n"),!.
ntest5_t :- typeof(iszero(succ(0)), 'Bool'),
	write_bt("ntest5_t passed.\n"),!.
ntest6_t :- typeof(iszero(pred(succ(0))), 'Bool'),
	write_bt("ntest6_t passed.\n"),!.
ntest7_t :- typeof(ifte(tru, 0, succ(0)), 'Natural'),
	write_bt("ntest7_t passed.\n"),!.

all_nat_type_tests_pass :-
	write_btt("--- Checking Naturals Type Tests. ---\n"),
	ntest1_t, ntest2_t, ntest3_t, ntest4_t, ntest5_t,
	ntest6_t, ntest7_t,
	write_btt("--- All Naturals Type Tests Pass. ---\n"),!.
/* ----- End Bool Tests ----- */
/* ----- Lambda Tests ----- */
% Abstraction
abstest1_t :- typeof(lam(X:'Bool', [X]), ('Bool'->'Bool')),
	write_bt("abstest1_t passed.\n"),!.
abstest2_t :- typeof(
	lam(X:'Bool', [lam(_:'Natural',[X])]),
	('Bool'->'Natural'->'Natural')),
	write_bt("abstest2_t passed.\n"),!.
abstest3_t :- typeof(
	lam(X:('Natural'->'Bool'), [lam(Y:'Natural',[X, Y])]),
	(('Natural'->'Bool')->'Natural'->'Bool')),
	write_bt("abstest3_t passed.\n"),!.
% Application
apptest1_t :- typeof([lam(X:'Bool', [X]), tru], 'Bool'),
	write_bt("apptest1_t passed.\n"),!.
apptest2_t :- typeof(
	[lam(X:('Natural'->'Bool'), [X]),
	lam(Y:'Natural', [iszero(Y)])],
	('Natural'->'Bool')),
	write_bt("apptest2_t passed.\n"),!.
apptest3_t :- typeof(
	[	lam(X:('Bool'->'Bool'), [lam(Z:'Bool',[X, Z])]),
		lam(Y:'Bool', [Y]),
		tru],
	'Bool'),
	write_bt("apptest3_t passed.\n"),!.

all_lambda_type_tests_pass :-
	write_btt("--- Checking Lambda Type Tests. ---\n"),
	abstest1_t, abstest2_t, abstest3_t,
	apptest1_t, apptest2_t, apptest3_t,
	write_btt("--- All Lambda Type Tests Pass. ---\n"),!.
/* ----- End Lambda Tests ----- */
/* ----- Tuple Tests ----- */
tpltest1_t :-  typeof(tuple([tru,0]), 'Tuple'(['Bool', 'Natural'])),
	write_bt("tpltest1_t passed.\n"),!.
tpltest2_t :-  typeof(
	tuple([tru,0,iszero(pred(succ(0))),ifte(tru, succ(0), 0)]),
	'Tuple'(['Bool', 'Natural','Bool','Natural'])),
	write_bt("tpltest2_t passed.\n"),!.
tpltest3_t :-  typeof(
	tuple([]),
	'Tuple'([])),
	write_bt("tpltest3_t passed.\n"),!.
tpltest4_t :-  typeof(
	tuple([lam(X:('Natural'->'Bool'), [lam(Y:'Natural',[X, Y])])]),
	'Tuple'([(('Natural'->'Bool')->'Natural'->'Bool')])),
	write_bt("tpltest4_t passed.\n"),!.

all_tuple_type_tests_pass :-
	write_btt("--- Checking Tuple Type Tests. ---\n"),
	tpltest1_t, tpltest2_t, tpltest3_t, tpltest4_t,
	write_btt("--- All Tuple Type Tests Pass. ---\n"),!.
/* ----- End Tuple Tests ----- */
/* ----- Record Tests ----- */
rcdtest1_t :-  typeof(
	record(["A"=tru,"B"=0]),
	'Record'(["A"='Bool', "B"='Natural'])),
	write_bt("rcdtest1_t passed.\n"),!.
rcdtest2_t :-  typeof(
	record(["A"=tru,"B"=0,"C"=iszero(pred(succ(0))),"D"=ifte(tru, succ(0), 0)]),
	'Record'(["A"='Bool', "B"='Natural',"C"='Bool',"D"='Natural'])),
	write_bt("rcdtest2_t passed.\n"),!.
rcdtest3_t :-  typeof(
	record([]),
	'Record'([])),
	write_bt("rcdtest3_t passed.\n"),!.
rcdtest4_t :-  typeof(
	record(["A"=lam(X:('Natural'->'Bool'), [lam(Y:'Natural',[X, Y])])]),
	'Record'(["A"=(('Natural'->'Bool')->'Natural'->'Bool')])),
	write_bt("rcdtest4_t passed.\n"),!.

all_record_type_tests_pass :-
	write_btt("--- Checking Record Type Tests. ---\n"),
	rcdtest1_t, rcdtest2_t, rcdtest3_t, rcdtest4_t,
	write_btt("--- All Record Type Tests Pass. ---\n"),!.
/* ----- End Tuple Tests ----- */
/* ----- List Tests ----- */
niltest_t :-  typeof(nil, 'List'(_)),
	write_bt("niltest_t passed.\n"),!.
constest1_t :-  typeof(cons(tru, nil), 'List'('Bool')),
	write_bt("constest1_t passed.\n"),!.
constest2_t :-  typeof(cons(0, nil), 'List'('Natural')),
	write_bt("constest2_t passed.\n"),!.
constest3_t :-  typeof(
	cons(succ(succ(0)), cons( succ(0), cons(0, nil))),
	'List'('Natural')),
	write_bt("constest3_t passed.\n"),!.
constest4_t :-  \+ typeof(
	cons(succ(succ(0)), cons( fls, cons(0, nil))),
	'List'(_)),
	write_bt("constest4_t passed.\n"),!.
isniltest1_t :-  typeof(isnil(nil), 'Bool'),
	write_bt("isniltest1_t passed.\n"),!.
isniltest2_t :-  typeof(isnil(cons(0, nil)), 'Bool'),
	write_bt("isniltest2_t passed.\n"),!.
isniltest3_t :-  \+ typeof(isnil(5), _),
	write_bt("isniltest3_t passed.\n"),!.
headtest1_t :-  typeof(head(nil), _),
	write_bt("headtest1_t passed.\n"),!.
headtest2_t :-  typeof(head(cons(0, nil)), 'Natural'),
	write_bt("headtest2_t passed.\n"),!.
headtest3_t :-  typeof(head(cons(fls, nil)), 'Bool'),
	write_bt("headtest3_t passed.\n"),!.
headtest4_t :-  \+ typeof(
	head(cons(succ(succ(0)), cons( fls, cons(0, nil)))),
	_),
	write_bt("headtest4_t passed.\n"),!.
tailtest1_t :-  typeof(tail(nil), 'List'(_)),
	write_bt("tailtest1_t passed.\n"),!.
tailtest2_t :-
	typeof(tail(cons(0, nil)), 'List'('Natural')),
		write_bt("tailtest2_t passed.\n"),!.
tailtest3_t :-
	typeof(tail(cons(fls, nil)), 'List'('Bool')),
		write_bt("tailtest3_t passed.\n"),!.
tailtest4_t :-
	typeof(tail(cons(tru, (cons(fls, nil)))), 'List'('Bool')),
		write_bt("tailtest4_t passed.\n"),!.
tailtest5_t :-  \+ typeof(
	tail(cons(succ(succ(0)), cons( fls, cons(0, nil)))),
	'List'(_)),
	write_bt("tailtest5_t passed.\n"),!.

all_list_type_tests_pass :-
	write_btt("--- Checking List Type Tests. ---\n"),
	niltest_t,
	constest1_t,constest2_t,constest3_t,constest4_t,
	isniltest1_t,isniltest2_t,isniltest3_t,
	headtest1_t,headtest2_t,headtest3_t,headtest4_t,
	tailtest1_t,tailtest2_t,tailtest3_t,tailtest4_t,tailtest5_t,
	write_btt("--- All List Type Tests Pass. ---\n"),!.
/* ----- End List Tests ----- */
/* ----- Exception Tests ----- */
exntest1_t :- true.

all_exception_type_tests_pass :-
	write_btt("--- Checking Exception Type Tests. ---\n"),
	exntest1_t,
	write_btt("--- All Exception Type Tests Pass. ---\n"),!.
/* ----- End Exception Tests ----- */

all_type_tests_pass :-
	all_unit_tests_pass,
	all_bool_type_tests_pass,
	all_nat_type_tests_pass,
	all_lambda_type_tests_pass,
	all_tuple_type_tests_pass,
	all_record_type_tests_pass,
	all_list_type_tests_pass,
	all_exception_type_tests_pass,!.
