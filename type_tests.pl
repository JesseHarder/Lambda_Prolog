:- [types, util/plists].

/* ----- Unit Tests ----- */
utest :- typeof(unit, 'Unit').
/* ----- End Unit Tests ----- */
/* ----- Bool Tests ----- */
btest1 :- typeof(tru, 'Bool'),!.
btest2 :- typeof(fls, 'Bool'),!.
btest3 :- typeof(ifte(tru, fls, fls), 'Bool'),!.
btest4 :- typeof(  ifte(ifte(tru,fls,fls),
						ifte(fls, fls, tru),
						ifte(tru, tru, ifte(tru,tru,tru))), 'Bool'),!.

all_bool_type_tests_pass :-
	btest1, btest2, btest3, btest4,!.
/* ----- End Bool Tests ----- */
/* ----- Natural Tests ----- */
ntest1 :- typeof(0, 'Natural'),!.
ntest2 :- typeof(succ(0), 'Natural'),!.
ntest3 :- typeof(pred(succ(0)), 'Natural'),!.
ntest4 :- typeof(iszero(0), 'Bool'),!.
ntest5 :- typeof(iszero(succ(0)), 'Bool'),!.
ntest6 :- typeof(iszero(pred(succ(0))), 'Bool'),!.
ntest7 :- typeof(ifte(tru, 0, succ(0)), 'Natural'),!.

all_nat_type_tests_pass :-
	ntest1, ntest2, ntest3, ntest4, ntest5,
	ntest6, ntest7,!.
/* ----- End Bool Tests ----- */
/* ----- Lambda Tests ----- */
% Abstraction
abstest1 :- typeof(lam(X:'Bool', [X]), ('Bool'->'Bool')),!.
abstest2 :- typeof(
	lam(X:'Bool', [lam(_:'Natural',[X])]),
	('Bool'->'Natural'->'Natural')),!.
abstest3 :- typeof(
	lam(X:('Natural'->'Bool'), [lam(Y:'Natural',[X, Y])]),
	(('Natural'->'Bool')->'Natural'->'Bool')),!.
% Application
apptest1 :- typeof([lam(X:'Bool', [X]), tru], 'Bool'),!.
apptest2 :- typeof(
	[lam(X:('Natural'->'Bool'), [X]),
	lam(Y:'Natural', [iszero(Y)])],
	('Natural'->'Bool')),!.
apptest3 :- typeof(
	[	lam(X:('Bool'->'Bool'), [lam(Z:'Bool',[X, Z])]),
		lam(Y:'Bool', [Y]),
		tru],
	'Bool'),!.

all_lambda_type_tests_pass :-
	abstest1, abstest2, abstest3,
	apptest1,apptest2,apptest3,!.
/* ----- End Lambda Tests ----- */
/* ----- Tuple Tests ----- */
tpltest1 :- typeof(tuple([tru,0]), 'Tuple'(['Bool', 'Natural'])),!.
tpltest2 :- typeof(
	tuple([tru,0,iszero(pred(succ(0))),ifte(tru, succ(0), 0)]),
	'Tuple'(['Bool', 'Natural','Bool','Natural'])),!.
tpltest3 :- typeof(
	tuple([]),
	'Tuple'([])),!.
tpltest4 :- typeof(
	tuple([lam(X:('Natural'->'Bool'), [lam(Y:'Natural',[X, Y])])]),
	'Tuple'([(('Natural'->'Bool')->'Natural'->'Bool')])),!.

all_tuple_type_tests_pass :-
	tpltest1, tpltest2, tpltest3, tpltest4.
/* ----- End Tuple Tests ----- */
/* ----- Record Tests ----- */
rcdtest1 :- typeof(
	record(["A"=tru,"B"=0]),
	'Record'(["A"='Bool', "B"='Natural'])),!.
rcdtest2 :- typeof(
	record(["A"=tru,"B"=0,"C"=iszero(pred(succ(0))),"D"=ifte(tru, succ(0), 0)]),
	'Record'(["A"='Bool', "B"='Natural',"C"='Bool',"D"='Natural'])),!.
rcdtest3 :- typeof(
	record([]),
	'Record'([])),!.
rcdtest4 :- typeof(
	record(["A"=lam(X:('Natural'->'Bool'), [lam(Y:'Natural',[X, Y])])]),
	'Record'(["A"=(('Natural'->'Bool')->'Natural'->'Bool')])),!.

all_record_type_tests_pass :-
	rcdtest1, rcdtest2, rcdtest3, rcdtest4.
/* ----- End Tuple Tests ----- */
/* ----- List Tests ----- */
niltest :- typeof(nil, 'List'(_)),!.
constest1 :- typeof(cons(tru, nil), 'List'('Bool')),!.
constest2 :- typeof(cons(0, nil), 'List'('Natural')),!.
constest3 :- typeof(
	cons(succ(succ(0)), cons( succ(0), cons(0, nil))),
	'List'('Natural')),!.
constest4 :- \+ typeof(
	cons(succ(succ(0)), cons( fls, cons(0, nil))),
	'List'(_)),!.
isniltest1 :- typeof(isnil(nil), 'Bool').
isniltest2 :- typeof(isnil(cons(0, nil)), 'Bool').
isniltest3 :- \+ typeof(isnil(5), _).
headtest1 :- typeof(head(nil), _).
headtest2 :- typeof(head(cons(0, nil)), 'Natural').
headtest3 :- typeof(head(cons(fls, nil)), 'Bool').
headtest4 :- \+ typeof(
	head(cons(succ(succ(0)), cons( fls, cons(0, nil)))),
	_),!.
tailtest1 :- typeof(tail(nil), 'List'(_)).
tailtest2 :-
	typeof(tail(cons(0, nil)), 'List'('Natural')).
tailtest3 :-
	typeof(tail(cons(fls, nil)), 'List'('Bool')).
tailtest4 :-
	typeof(tail(cons(tru, (cons(fls, nil)))), 'List'('Bool')).
tailtest5 :- \+ typeof(
	tail(cons(succ(succ(0)), cons( fls, cons(0, nil)))),
	'List'(_)),!.

all_list_type_tests_pass :-
	niltest,
	constest1,constest2,constest3,constest4,
	isniltest1,isniltest2,isniltest3,
	headtest1,headtest2,headtest3,headtest4,
	tailtest1,tailtest2,tailtest3,tailtest4,tailtest5,!.
/* ----- End List Tests ----- */

all_type_tests_pass :-
	utest,
	all_bool_type_tests_pass,
	all_nat_type_tests_pass,
	all_lambda_type_tests_pass,
	all_tuple_type_tests_pass,
	all_record_type_tests_pass,!.
