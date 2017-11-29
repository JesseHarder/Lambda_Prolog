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
apptest1 :- typeof([lam(X:'Bool', [X]), tru], 'Bool').
apptest2 :- typeof(
	[lam(X:('Natural'->'Bool'), [X]),
	lam(Y:'Natural', [iszero(Y)])],
	('Natural'->'Bool')).
apptest3 :- typeof(
	[	lam(X:('Bool'->'Bool'), [lam(Z:'Bool',[X, Z])]),
		lam(Y:'Bool', [Y]),
		tru],
	'Bool').

all_lambda_type_tests_pass :-
	abstest1, abstest2, abstest3,
	apptest1,apptest2,apptest3,!.
/* ----- End Lambda Tests ----- */

all_type_tests_pass :-
	utest,
	all_bool_type_tests_pass,
	all_nat_type_tests_pass,
	all_lambda_type_tests_pass,!.
