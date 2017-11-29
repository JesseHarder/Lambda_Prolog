:- [types, util/plists].

/* ----- Bool Tests ----- */
btest1 :- typeof(tru, 'Bool').
btest2 :- typeof(fls, 'Bool').
btest3 :- typeof(ifte(tru, fls, fls), 'Bool').
btest4 :- typeof(  ifte(ifte(tru,fls,fls),
						ifte(fls, fls, tru),
						ifte(tru, tru, ifte(tru,tru,tru))), 'Bool').

run_all_bool_tests :- btest1, btest2, btest3, btest4.

run_all_tests :- run_all_bool_tests.
