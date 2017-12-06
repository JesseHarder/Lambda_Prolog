:- [eval, values, util/plists, util/test_controls].

/* ----- Bool Tests ----- */
btest1_e :- eval(ifte(tru, tru, fls), tru),
	write_bt("btest1_e passed.\n"),!.
btest2_e :- eval(ifte(fls, tru, fls), fls),
	write_bt("btest2_e passed.\n"),!.
btest3_e :-
	eval(ifte(
		ifte(fls,fls,tru),
		ifte(tru,tru,fls),
		ifte(tru,fls,tru)),
	tru),
	write_bt("btest3_e passed.\n"),!.

all_bool_eval_tests_pass :-
	write_btt("--- Checking Boolean Eval Tests. ---\n"),
	btest1_e, btest2_e, btest3_e,
	write_btt("--- All Boolean Eval Tests Pass. ---\n"),!.
/* ----- End Bool Tests ----- */

/* ----- Natural Tests ----- */
% E-PredZero Test
ntest1_e :- eval(pred(0), 0),
	write_bt("ntest1_e passed.\n"),!.
% E-PredSucc Tests
ntest2_e :- eval(pred(succ(0)), 0),
	write_bt("ntest2_e passed.\n"),!.
ntest3_e :- eval(pred(succ(succ(0))), succ(0)),
	write_bt("ntest3_e passed.\n"),!.
ntest4_e :- eval(pred(succ(ifte(tru,succ(0),0))), succ(0)),
	write_bt("ntest4_e passed.\n"),!.
% E-Succ Test
ntest5_e :- eval(succ(ifte(tru,0,succ(0))), succ(0)),
	write_bt("ntest5_e passed.\n"),!.
% E-IsZeroZero Test
ntest6_e :- eval(iszero(0), tru),
	write_bt("ntest6_e passed.\n"),!.
% E-IsZeroSucc Test
ntest7_e :- eval(iszero(succ(0)), fls),
	write_bt("ntest7_e passed.\n"),!.
% E-IsZero Test
ntest8_e :- eval(iszero(ifte(tru,0,succ(0))), tru),
	write_bt("ntest8_e passed.\n"),!.

all_nat_eval_tests_pass :-
	write_btt("--- Checking Natural Eval Tests. ---\n"),
	ntest1_e, ntest2_e, ntest3_e, ntest4_e,
	ntest5_e, ntest6_e, ntest7_e, ntest8_e,
	write_btt("--- All Natural Eval Tests Pass. ---\n"),!.
/* ----- End Natural Tests ----- */

/* ----- Lambda Tests ----- */
% E-AppAbs-2
lamtest1_e :- eval([lam(X:'Bool',X), tru], tru),
	write_bt("lamtest1_e passed.\n"),!.
lamtest2_e :- eval([lam(X:'Natural',X), 0], 0),
	write_bt("lamtest2_e passed.\n"),!.
lamtest3_e :-
	eval(
		[lam(X:('Natural'->'Natural'),X),
		lam(Y:'Natural', Y)],
	lam(Y:'Natural', Y)),
	write_bt("lamtest3_e passed.\n"),!.
% E-AppAbs-3
lamtest4_e :-
	eval(
		[lam(X:('Natural'->'Natural'),X),
		lam(Y:'Natural', Y),
		succ(0)],
	succ(0)),
	write_bt("lamtest4_e passed.\n"),!.
% E-App2
lamtest5_e :- eval([lam(X:'Bool',X), iszero(0)], tru),
	write_bt("lamtest5_e passed.\n"),!.
% E-App1
lamtest6_e :-
	eval([ifte(
		tru,
		lam(X:'Natural',X),
		lam(Y:'Natural',succ(Y))),
	0],
	0),
	write_bt("lamtest6_e passed.\n"),!.

all_lambda_eval_tests_pass :-
	write_btt("--- Checking Lambda Eval Tests. ---\n"),
	lamtest1_e, lamtest2_e, lamtest3_e, lamtest4_e,
	lamtest5_e, lamtest6_e,
	write_btt("--- All Lambda Eval Tests Pass. ---\n"),!.
/* ----- End Lambda Tests ----- */

/* ----- Unit Tests ----- */
% E-AppAbs-2
unittest1_e :-
	eval([lam(_:'Unit', tru), unit], tru),
	write_bt("unittest1_e passed.\n"),!.
unittest2_e :-
	eval([lam(X:'Unit', X), unit], unit),
	write_bt("unittest2_e passed.\n"),!.
seqtest1_e :- eval(seq([iszero(0), iszero(succ(0))]), fls),
	write_bt("seqtest1_e passed.\n"),!.
seqtest2_e :- eval(seq([ifte(tru, fls, fls), ifte(fls, fls, tru)]), tru),
	write_bt("seqtest2_e passed.\n"),!.
seqtest3_e :-
	eval(seq([iszero(succ(0)),
			  ifte(tru, tru, fls),
			  [lam(X:'Unit', X), unit]]),
		 unit),
	write_bt("seqtest3_e passed.\n"),!.

all_unit_eval_tests_pass :-
	write_btt("--- Checking Unit Eval Tests. ---\n"),
	unittest1_e, unittest2_e,
	seqtest1_e, seqtest2_e, seqtest3_e,
	write_btt("--- All Unit Eval Tests Pass. ---\n"),!.
/* ----- End Unit Tests ----- */

/* ----- Let Tests ----- */
% E-AppAbs-2
lettest1_e :- eval(let(X=0, iszero(X)), tru),
	write_bt("lettest1_e passed.\n"),!.
lettest2_e :- eval(let(Y=tru, ifte(Y, 0, succ(0))), 0),
	write_bt("lettest2_e passed.\n"),!.
lettest3_e :- eval(let(Y=fls, [lam(X:'Natural', ifte(Y, 0, X)), succ(0)]), succ(0)),
	write_bt("lettest3_e passed.\n"),!.

all_let_eval_tests_pass :-
	write_btt("--- Checking Let Eval Tests. ---\n"),
	lettest1_e, lettest2_e, lettest3_e,
	write_btt("--- All Let Eval Tests Pass. ---\n"),!.
/* ----- End Let Tests ----- */

/* ----- Fix Tests ----- */
% E-AppAbs-2
fixtest1_e :- eval(fix(lam(X:'Bool',X)), _),
	write_bt("fixtest1_e passed.\n"),!.

all_fix_eval_tests_pass :-
	write_btt("--- Checking Fix Eval Tests. ---\n"),
	fixtest1_e,
	write_btt("--- All Fix Eval Tests Pass. ---\n"),!.
/* ----- End Fix Tests ----- */

all_eval_tests_pass :-
	all_bool_eval_tests_pass,
	all_nat_eval_tests_pass,
	all_lambda_eval_tests_pass,
	all_unit_eval_tests_pass,
	all_let_eval_tests_pass.
	% all_fix_eval_tests_pass.
