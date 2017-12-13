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
lamtest1_e :- eval([lam(x:'Bool',x), tru], tru),
	write_bt("lamtest1_e passed.\n"),!.
lamtest2_e :- eval([lam(x:'Nat',x), 0], 0),
	write_bt("lamtest2_e passed.\n"),!.
lamtest3_e :-
	eval(
		[lam(x:('Nat'->'Nat'),x),
		lam(y:'Nat', y)],
	lam(y:'Nat', y)),
	write_bt("lamtest3_e passed.\n"),!.
% E-AppAbs-3
lamtest4_e :-
	eval(
		[lam(x:('Nat'->'Nat'),x),
		lam(y:'Nat', y),
		succ(0)],
	succ(0)),
	write_bt("lamtest4_e passed.\n"),!.
% E-App2
lamtest5_e :- eval([lam(x:'Bool',x), iszero(0)], tru),
	write_bt("lamtest5_e passed.\n"),!.
% E-App1
lamtest6_e :-
	eval([ifte(
		tru,
		lam(x:'Nat',x),
		lam(y:'Nat',succ(y))),
	0],
	0),
	write_bt("lamtest6_e passed.\n"),!.
lamtest7_e :-
	eval(
		[lam(y:'Nat', pred(y)),
		succ(0)],
	0),
	write_bt("lamtest7_e passed.\n"),!.
lamtest8_e :-
	eval([lam(y:('Nat'->'Nat'->'Nat'), lam(y:'Nat', y)), 0],
		lam(y:'Nat', (y))),
	write_bt("lamtest8_e passes.\n"),!.



all_lambda_eval_tests_pass :-
	write_btt("--- Checking Lambda Eval Tests. ---\n"),
	lamtest1_e, lamtest2_e, lamtest3_e, lamtest4_e,
	lamtest5_e, lamtest6_e, lamtest7_e, lamtest8_e,
	write_btt("--- All Lambda Eval Tests Pass. ---\n"),!.
/* ----- End Lambda Tests ----- */

/* ----- Unit Tests ----- */
% E-AppAbs-2
unittest1_e :-
	eval([lam(x:'Unit', tru), unit], tru),
	write_bt("unittest1_e passed.\n"),!.
unittest2_e :-
	eval([lam(x:'Unit', x), unit], unit),
	write_bt("unittest2_e passed.\n"),!.
seqtest1_e :- eval(seq([iszero(0), iszero(succ(0))]), fls),
	write_bt("seqtest1_e passed.\n"),!.
seqtest2_e :- eval(seq([ifte(tru, fls, fls), ifte(fls, fls, tru)]), tru),
	write_bt("seqtest2_e passed.\n"),!.
seqtest3_e :-
	eval(seq([iszero(succ(0)),
			  ifte(tru, tru, fls),
			  [lam(x:'Unit', x), unit]]),
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
lettest1_e :- eval(let(x=0, iszero(x)), tru),
	write_bt("lettest1_e passed.\n"),!.
lettest2_e :- eval(let(y=tru, ifte(y, 0, succ(0))), 0),
	write_bt("lettest2_e passed.\n"),!.
lettest3_e :- eval(let(y=fls, [lam(x:'Nat', ifte(y, 0, x)), succ(0)]), succ(0)),
	write_bt("lettest3_e passed.\n"),!.

all_let_eval_tests_pass :-
	write_btt("--- Checking Let Eval Tests. ---\n"),
	lettest1_e, lettest2_e, lettest3_e,
	write_btt("--- All Let Eval Tests Pass. ---\n"),!.
/* ----- End Let Tests ----- */

/* ----- Tuple Tests ----- */
tpltest1_e :-  eval(tuple([tru,iszero(0)]), tuple([tru,tru])),
	write_bt("tpltest1_e passed.\n"),!.
tpltest2_e :-  eval(
	tuple([tru,0,iszero(pred(succ(0))),ifte(tru, succ(0), 0)]),
	tuple([tru,0,tru,succ(0)])),
	write_bt("tpltest2_e passed.\n"),!.
tprjtest1_e :-  eval(proj(tuple([0, tru]), 1), 0),
	write_bt("projtest1_e passed.\n"),!.
tprjtest2_e :-  eval(proj(tuple([0, tru]), 2), tru),
	write_bt("projtest2_e passed.\n"),!.
tprjtest3_e :-  eval(proj(tuple([iszero(0), ifte(fls, 0, succ(0))]), 1), tru),
	write_bt("projtest3_e passed.\n"),!.
tprjtest4_e :-  eval(proj(tuple([iszero(0), ifte(fls, 0, succ(0))]), 2), succ(0)),
	write_bt("projtest4_e passed.\n"),!.


all_tuple_eval_tests_pass :-
	write_btt("--- Checking Tuple Eval Tests. ---\n"),
	tpltest1_e, tpltest2_e,
	rprjtest1_e, tprjtest2_e, tprjtest3_e, tprjtest4_e,
	write_btt("--- All Tuple Eval Tests Pass. ---\n"),!.
/* ----- End Tuple Tests ----- */

/* ----- Record Tests ----- */
rcdtest1_e :-  eval(record(["A"=tru,"B"=iszero(0)]), record(["A"=tru,"B"=tru])),
	write_bt("rcdtest1_e passed.\n"),!.
rcdtest2_e :-  eval(
	record(["A"=tru,"B"=0,"C"=iszero(pred(succ(0))),"D"=ifte(tru, succ(0), 0)]),
	record(["A"=tru,"B"=0,"C"=tru,"D"=succ(0)])),
	write_bt("rcdtest2_e passed.\n"),!.
rprjtest1_e :-  eval(proj(record(["A"=0, "B"=tru]), "A"), 0),
	write_bt("projtest1_e passed.\n"),!.
rprjtest2_e :-  eval(proj(record(["A"=0, "B"=tru]), "B"), tru),
	write_bt("projtest2_e passed.\n"),!.
rprjtest3_e :-  eval(proj(record(["A"=iszero(0), "B"=ifte(fls, 0, succ(0))]), "A"), tru),
	write_bt("projtest3_e passed.\n"),!.
rprjtest4_e :-  eval(proj(record(["A"=iszero(0), "B"=ifte(fls, 0, succ(0))]), "B"), succ(0)),
	write_bt("projtest4_e passed.\n"),!.


all_record_eval_tests_pass :-
	write_btt("--- Checking Record Eval Tests. ---\n"),
	rcdtest1_e, rcdtest2_e,
	rprjtest1_e, rprjtest2_e, rprjtest3_e, rprjtest4_e,
	write_btt("--- All Record Eval Tests Pass. ---\n"),!.
/* ----- End Record Tests ----- */

/* ----- Variant Tests ----- */
vrnttest1_e :-
	eval(case(var("A"=tru),
				[var("A"=x)->ifte(x,0,succ(0)),
				 var("B"=y)->[lam(z:'Nat',iszero(z)), y]]),
			0),
	write_bt("vrnttest1_e passed.\n"),!.
vrnttest2_e :-
	\+ eval(case(var("B"=tru),
				[var("A"=x)->ifte(x,0,succ(0)),
				 var("B"=y)->[lam(z:'Nat', iszero(z)), y]]),
			_),
	write_bt("vrnttest2_e passed.\n"),!.
vrnttest3_e :-
	eval(case(var("B"=0),
				[var("A"=x)->ifte(x,0,succ(0)),
				 var("B"=y)->[lam(z:'Nat',iszero(z)), y]]),
			tru),
	write_bt("vrnttest3_e passed.\n"),!.
vrnttest4_e :-
	eval(case(var("B"=0),
				[var("A"=x)->ifte(x,0,succ(0)),
				 var("B"=y)->lam(x:'Nat',iszero(y))]),
			lam(x:'Nat',iszero(0))),
	write_bt("vrnttest4_e passed.\n"),!.

all_variant_eval_tests_pass :-
	write_btt("--- Checking Variant Eval Tests. ---\n"),
	vrnttest1_e, vrnttest2_e, vrnttest3_e, vrnttest4_e,
	write_btt("--- All Variant Eval Tests Pass. ---\n"),!.
/* ----- End Variant Tests ----- */

/* ----- List Tests ----- */
isniltest1_e :-  eval(isnil(nil), tru),
	write_bt("isniltest1_e passed.\n"),!.
isniltest2_e :-  eval(isnil(cons(0, nil)), fls),
	write_bt("isniltest2_e passed.\n"),!.
isniltest3_e :-  \+ eval(isnil(5), _),
	write_bt("isniltest3_e passed.\n"),!.
% This types, but it doesn't evaluate. Should it? Or should it not type?
headtest1_e :-  % \+ eval(head(nil), _),
	write_bt("headtest1_e infinite loops. <-- NOTE!\n"),!.
headtest2_e :-  eval(head(cons(0, nil)), 0),
	write_bt("headtest2_e passed.\n"),!.
headtest3_e :-  eval(head(cons(fls, nil)), fls),
	write_bt("headtest3_e passed.\n"),!.
% Note: This one evaluates, but won't type, which is correct.
headtest4_e :-  eval(
	head(cons(succ(succ(0)), cons( fls, cons(0, nil)))),
	succ(succ(0))),
	write_bt("headtest4_e passed.\n"),!.
	% This types, but it doesn't evaluate. Should it? Or should it not type?
tailtest1_e :-  % \+ eval(tail(nil), _),
	write_bt("tailtest1_e infinite loops. <-- NOTE!\n"),!.
tailtest2_e :-
	eval(tail(cons(0, nil)), nil),
		write_bt("tailtest2_e passed.\n"),!.
tailtest3_e :-
	eval(tail(cons(fls, nil)), nil),
		write_bt("tailtest3_e passed.\n"),!.
tailtest4_e :-
	eval(tail(cons(tru, (cons(fls, nil)))), cons(fls, nil)),
		write_bt("tailtest4_e passed.\n"),!.
% Note: This one evaluates, but won't type, which is correct.
tailtest5_e :-  eval(
	tail(cons(succ(succ(0)), cons( fls, cons(0, nil)))),
	cons( fls, cons(0, nil))),
	write_bt("tailtest5_e passed.\n"),!.

all_list_eval_tests_pass :-
	write_btt("--- Checking List Eval Tests. ---\n"),
	isniltest1_e,isniltest2_e,isniltest3_e,
	headtest1_e,headtest2_e,headtest3_e,headtest4_e,
	tailtest1_e,tailtest2_e,tailtest3_e,tailtest4_e,tailtest5_e,
	write_btt("--- All List Eval Tests Pass. ---\n"),!.
/* ----- End List Tests ----- */

/* ----- Exception Tests ----- */
errtest1_e :- eval([error, 0], error),
	write_bt("exntest1_e passed.\n"),!.
errtest2_e :- eval([lam(x:'Bool', x), error], error),
	write_bt("exntest2_e passed.\n"),!.
errtest3_e :- eval(try(0, fls), 0),
	write_bt("exntest3_e passed.\n"),!.
errtest4_e :- eval(try([error, 0], iszero(0)), tru),
	write_bt("exntest4_e passed.\n"),!.
raisetest1_e :- eval([raise(0), 0], raise(0)),
	write_bt("raisetest1_e passed.\n"),!.
raisetest2_e :- eval([raise(raise(0)), 0], raise(0)),
	write_bt("raisetest2_e passed.\n"),!.
raisetest3_e :- eval([lam(x:'Bool', x), raise(0)], raise(0)),
	write_bt("raisetest3_e passed.\n"),!.
raisetest4_e :- eval(try([raise(0), 0], lam(x:'Nat', iszero(x))), tru),
	write_bt("raisetest4_e passed.\n"),!.

all_exception_eval_tests_pass :-
	write_btt("--- Checking Exception Eval Tests. ---\n"),
	errtest1_e, errtest2_e, errtest3_e, errtest4_e,
	raisetest1_e, raisetest2_e, raisetest3_e, raisetest4_e,
	write_btt("--- All Exception Eval Tests Pass. ---\n"),!.
/* ----- End Exception Tests ----- */

/* ----- Fix Tests ----- */
fixtest1_e :- eval(fix(lam(x:'Bool',x)), _),
	write_bt("fixtest1_e passed.\n"),!.

fix_iseven_test(Result) :-
	eval(
		let(ff,
		 	lam(ie:('Natural'->'Bool'),
		 		lam(x:'Natural',
					ifte(
						iszero(x),
						tru,
						ifte(
							iszero(pred(x)),
							fls,
							[ie, pred(pred(x))]
							)
						)
					)
				),
			let(iseven,
				fix(ff),
				[iseven, 7],
				Result)
			),
		Result
		).

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
	all_let_eval_tests_pass,
	all_tuple_eval_tests_pass,
	all_record_eval_tests_pass,
	all_variant_eval_tests_pass,
	all_list_eval_tests_pass,
	all_exception_eval_tests_pass.
	% all_fix_eval_tests_pass.
