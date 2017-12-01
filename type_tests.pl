:- [types, util/plists].

/* ----- Unit Tests ----- */
utest_t :-  typeof(unit, 'Unit').
/* ----- End Unit Tests ----- */
/* ----- Bool Tests ----- */
btest1_t :- typeof(tru, 'Bool'),!.
btest2_t :- typeof(fls, 'Bool'),!.
btest3_t :- typeof(ifte(tru, fls, fls), 'Bool'),!.
btest4_t :- typeof(  ifte(ifte(tru,fls,fls),
						ifte(fls, fls, tru),
						ifte(tru, tru, ifte(tru,tru,tru))), 'Bool'),!.

all_bool_type_tests_pass :-
	btest1_t, btest2_t, btest3_t, btest4_t,!.
/* ----- End Bool Tests ----- */
/* ----- Natural Tests ----- */
ntest1_t :- typeof(0, 'Natural'),!.
ntest2_t :- typeof(succ(0), 'Natural'),!.
ntest3_t :- typeof(pred(succ(0)), 'Natural'),!.
ntest4_t :- typeof(iszero(0), 'Bool'),!.
ntest5_t :- typeof(iszero(succ(0)), 'Bool'),!.
ntest6_t :- typeof(iszero(pred(succ(0))), 'Bool'),!.
ntest7_t :- typeof(ifte(tru, 0, succ(0)), 'Natural'),!.

all_nat_type_tests_pass :-
	ntest1_t, ntest2_t, ntest3_t, ntest4_t, ntest5_t,
	ntest6_t, ntest7_t,!.
/* ----- End Bool Tests ----- */
/* ----- Lambda Tests ----- */
% Abstraction
abstest1_t :- typeof(lam(X:'Bool', [X]), ('Bool'->'Bool')),!.
abstest2_t :- typeof(
	lam(X:'Bool', [lam(_:'Natural',[X])]),
	('Bool'->'Natural'->'Natural')),!.
abstest3_t :- typeof(
	lam(X:('Natural'->'Bool'), [lam(Y:'Natural',[X, Y])]),
	(('Natural'->'Bool')->'Natural'->'Bool')),!.
% Application
apptest1_t :- typeof([lam(X:'Bool', [X]), tru], 'Bool'),!.
apptest2_t :- typeof(
	[lam(X:('Natural'->'Bool'), [X]),
	lam(Y:'Natural', [iszero(Y)])],
	('Natural'->'Bool')),!.
apptest3_t :- typeof(
	[	lam(X:('Bool'->'Bool'), [lam(Z:'Bool',[X, Z])]),
		lam(Y:'Bool', [Y]),
		tru],
	'Bool'),!.

all_lambda_type_tests_pass :-
	abstest1_t, abstest2_t, abstest3_t,
	apptest1_t, apptest2_t, apptest3_t,!.
/* ----- End Lambda Tests ----- */
/* ----- Tuple Tests ----- */
tpltest1_t :-  typeof(tuple([tru,0]), 'Tuple'(['Bool', 'Natural'])),!.
tpltest2_t :-  typeof(
	tuple([tru,0,iszero(pred(succ(0))),ifte(tru, succ(0), 0)]),
	'Tuple'(['Bool', 'Natural','Bool','Natural'])),!.
tpltest3_t :-  typeof(
	tuple([]),
	'Tuple'([])),!.
tpltest4_t :-  typeof(
	tuple([lam(X:('Natural'->'Bool'), [lam(Y:'Natural',[X, Y])])]),
	'Tuple'([(('Natural'->'Bool')->'Natural'->'Bool')])),!.

all_tuple_type_tests_pass :-
	tpltest1_t, tpltest2_t, tpltest3_t, tpltest4_t.
/* ----- End Tuple Tests ----- */
/* ----- Record Tests ----- */
rcdtest1_t :-  typeof(
	record(["A"=tru,"B"=0]),
	'Record'(["A"='Bool', "B"='Natural'])),!.
rcdtest2_t :-  typeof(
	record(["A"=tru,"B"=0,"C"=iszero(pred(succ(0))),"D"=ifte(tru, succ(0), 0)]),
	'Record'(["A"='Bool', "B"='Natural',"C"='Bool',"D"='Natural'])),!.
rcdtest3_t :-  typeof(
	record([]),
	'Record'([])),!.
rcdtest4_t :-  typeof(
	record(["A"=lam(X:('Natural'->'Bool'), [lam(Y:'Natural',[X, Y])])]),
	'Record'(["A"=(('Natural'->'Bool')->'Natural'->'Bool')])),!.

all_record_type_tests_pass :-
	rcdtest1_t, rcdtest2_t, rcdtest3_t, rcdtest4_t.
/* ----- End Tuple Tests ----- */
/* ----- List Tests ----- */
niltest_t :-  typeof(nil, 'List'(_)),!.
constest1_t :-  typeof(cons(tru, nil), 'List'('Bool')),!.
constest2_t :-  typeof(cons(0, nil), 'List'('Natural')),!.
constest3_t :-  typeof(
	cons(succ(succ(0)), cons( succ(0), cons(0, nil))),
	'List'('Natural')),!.
constest4_t :-  \+ typeof(
	cons(succ(succ(0)), cons( fls, cons(0, nil))),
	'List'(_)),!.
isniltest1_t :-  typeof(isnil(nil), 'Bool').
isniltest2_t :-  typeof(isnil(cons(0, nil)), 'Bool').
isniltest3_t :-  \+ typeof(isnil(5), _).
headtest1_t :-  typeof(head(nil), _).
headtest2_t :-  typeof(head(cons(0, nil)), 'Natural').
headtest3_t :-  typeof(head(cons(fls, nil)), 'Bool').
headtest4_t :-  \+ typeof(
	head(cons(succ(succ(0)), cons( fls, cons(0, nil)))),
	_),!.
tailtest1_t :-  typeof(tail(nil), 'List'(_)).
tailtest2_t :-
	typeof(tail(cons(0, nil)), 'List'('Natural')).
tailtest3_t :-
	typeof(tail(cons(fls, nil)), 'List'('Bool')).
tailtest4_t :-
	typeof(tail(cons(tru, (cons(fls, nil)))), 'List'('Bool')).
tailtest5_t :-  \+ typeof(
	tail(cons(succ(succ(0)), cons( fls, cons(0, nil)))),
	'List'(_)),!.

all_list_type_tests_pass :-
	niltest_t,
	constest1_t,constest2_t,constest3_t,constest4_t,
	isniltest1_t,isniltest2_t,isniltest3_t,
	headtest1_t,headtest2_t,headtest3_t,headtest4_t,
	tailtest1_t,tailtest2_t,tailtest3_t,tailtest4_t,tailtest5_t,!.
/* ----- End List Tests ----- */

all_type_tests_pass :-
	utest_t,
	all_bool_type_tests_pass,
	all_nat_type_tests_pass,
	all_lambda_type_tests_pass,
	all_tuple_type_tests_pass,
	all_record_type_tests_pass,
	all_list_type_tests_pass,!.
