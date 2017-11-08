/*
 * Prolog code to be used to write type-checkable prolog code.
 *
 * Note, existing type checking predicates in prolog are listed here:
 * http://pauillac.inria.fr/~haemmerl/gprolog-rh/doc/manual024.html
 *
 */

/* Predicate: ifthenelse(Condition, T1, T2, Result)
* If Condition = tru, then Result = T1
* If Condition = fls, then Result = T2
*/
ifthenelse(tru, T1, _, T1).
ifthenelse(fls, _, T2, T2).

/* Math */
add(X,Y,S) :- S is X+Y.
sub(X,Y,D) :- D is X-Y.
mul(X,Y,P) :- P is X*Y.
div(N,D,Q) :- Q is N/D.

/* ^^^^^ KNOWLEDGE BASE ABOVE ^^^^^*/
/* VVVVVVVVV TYPING BELOW VVVVVVVVV*/

/* ----- Atom and Variable Types ----- */
% Booleans
type(tru, bool).
type(fls, bool).
% Numbers - Anything instatiated to a number has type "number".
type(X, number) :- number(X).
% Variables - If X is unistatiated, it could potentially be of any type.
type(X, _) :- var(X).

/* ----- Predicate Types ----- */
/* -- Booleans -- */
% ifthenelse: [bool, T, T, T]
type(ifthenelse(A,B,C,D),[bool,T2,T2,T2]) :-
	type(A, bool),
	type(B, T2),
	type(C, T2),
	type(D, T2).
/* -- Numbers -- */
% add: [number, number, number]
type(add(X,Y,S),[number,number,number]) :-
	type(X, T1),
	type(Y, T1),
	type(S, T1),
	T1 = number.
