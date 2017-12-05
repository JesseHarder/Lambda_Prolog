:- [types, util/plists, util/test_controls].

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
/* ----- Let Tests ----- */
% Abstraction
lettest1_t :- typeof(
	let(X=lam(Y:'Bool',[Y]),X),
	('Bool'->'Bool')),
	write_bt("lettest1_t passed.\n"),!.
lettest2_t :- typeof(
	let(X=lam(Y:'Bool',[Y]),[X,tru]),
	'Bool'),
	write_bt("lettest2_t passed.\n"),!.
lettest3_t :- typeof(
	let(X=lam(Y:'Natural',[iszero(Y)]),[X,0]),
	'Bool'),
	write_bt("lettest3_t passed.\n"),!.
lettest4_t :- typeof(
	let(X=tru, ifte(X,0, succ(0))),
	'Natural'),
	write_bt("lettest4_t passed.\n"),!.

all_let_type_tests_pass :-
	write_btt("--- Checking Let Type Tests. ---\n"),
	lettest1_t, lettest2_t, lettest3_t, lettest4_t,
	write_btt("--- All Let Type Tests Pass. ---\n"),!.
/* ----- End Let Tests ----- */
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
/* ----- End Records Tests ----- */
/* ----- Variant Tests ----- */
vrnttest1_t :-  typeof(vrnt("Hi"=0), 'Variant'(["Hi"='Natural'|_])),
	write_bt("vrnttest1_t passed.\n"),!.
vrnttest2_t :-
	typeof(vrnt("Hi"=record(["A"=tru,"B"=0])),
		'Variant'(["Hi"='Record'(["A"='Bool', "B"='Natural'])|_])),
	write_bt("vrnttest2_t passed.\n"),!.
vrnttest3_t :-
	typeof(case(var("A"=tru),
				[var("A"=X)->ifte(X,0,succ(0)),
				 var("B"=Y)->lam(Y,[iszero(Y)])]),
			'Natural'),
	write_bt("vrnttest3_t passed.\n"),!.
vrnttest4_t :-
	\+ typeof(case(var("B"=tru),
				[var("A"=X)->ifte(X,0,succ(0)),
				 var("B"=Y)->lam(Y:'Bool',[iszero(Y)])]),
			_),
	write_bt("vrnttest4_t passed.\n"),!.
vrnttest5_t :-
	typeof(case(var("B"=0),
				[var("A"=X)->ifte(X,0,succ(0)),
				 var("B"=Y)->lam(Y:'Natural',[iszero(Y)])]),
			_),
	write_bt("vrnttest5_t passed.\n"),!.

all_vrntiant_type_tests_pass :-
	write_btt("--- Checking Variant Type Tests. ---\n"),
	vrnttest1_t, vrnttest2_t, vrnttest3_t, vrnttest4_t, vrnttest5_t,
	write_btt("--- All Variant Type Tests Pass. ---\n"),!.
/* ----- End Variant Tests ----- */
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
exntest1_t :- typeof(raise(0), _),
	write_bt("exntest1_t passed.\n"),!.
exntest2_t :- \+ typeof(raise(tru), _),
	write_bt("exntest2_t passed.\n"),!.
exntest3_t :- typeof(try(raise(0), lam(X:'Natural',[X])), 'Natural'),
	write_bt("exntest3_t passed.\n"),!.
exntest4_t :- \+ typeof(try(raise(tru), lam(X:'Natural',[X])), _),
	write_bt("exntest4_t passed.\n"),!.
exntest5_t :- \+ typeof(try(raise(tru), lam(X:'Bool',[X])), _),
	write_bt("exntest5_t passed.\n"),!.

all_exception_type_tests_pass :-
	write_btt("--- Checking Exception Type Tests. ---\n"),
	write_btt("NOTE: These tests were written for when T_Exn is 'Natural'.\n"),
	exntest1_t, exntest2_t, exntest3_t, exntest4_t, exntest5_t,
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
