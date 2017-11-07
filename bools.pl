/*
 * Prolog code to emulate the behavior of booleans
 *
 * Types introduced in the file:
 * 		bool
 */

[types].

tru.
fls.

/* Predicate: ifthenelse(Condition, T1, T2, Result)
 * If Condition = tru, then Result = T1
 * If Condition = fls, then Result = T2
 */
 ifthenelse(tru, T1, _, T1).
 ifthenelse(fls, _, T2, T2).

/* Typing rules */
type(tru, bool).
type(fls, bool).
type(ifthenelse(A,B,C,_),[T1,T2,T3,T2]) :-
	type(A, T1),
	type(B, T2),
	type(C, T3).
