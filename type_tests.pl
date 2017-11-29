:- [types, util/plists].

/* ----- Unit Tests ----- */
/* ----- End Unit Tests ----- */

/* ----- Bool Tests ----- */
btest1 :- typeof(tru, 'Bool'),!.
btest2 :- typeof(fls, 'Bool'),!.
btest3 :- typeof(ifte(tru, fls, fls), 'Bool'),!.
btest4 :- typeof(  ifte(ifte(tru,fls,fls),
						ifte(fls, fls, tru),
						ifte(tru, tru, ifte(tru,tru,tru))), 'Bool'),!.

run_all_bool_tests :- btest1, btest2, btest3, btest4,!.
/* ----- End Bool Tests ----- */
/* ----- Natural Tests ----- */
ntest1 :- typeof(0, 'Natural'),!.
ntest2 :- typeof(succ(0), 'Natural'),!.
ntest3 :- typeof(pred(succ(0)), 'Natural'),!.
ntest4 :- typeof(iszero(0), 'Bool'),!.
ntest5 :- typeof(iszero(succ(0)), 'Bool'),!.
ntest6 :- typeof(iszero(pred(succ(0))), 'Bool'),!.
ntest7 :- typeof(ifte(tru, 0, succ(0)), 'Natural'),!.

run_all_nat_tests :-
	ntest1, ntest2, ntest3, ntest4, ntest5,
	ntest6, ntest7,!.


/* ----- End Natural Tests ----- */

run_all_tests :- run_all_bool_tests,!.
