/*
 * Prolog code to be used to write type-checkable prolog code.
 *
 * Note, existing type checking predicates in prolog are listed here:
 * http://pauillac.inria.fr/~haemmerl/gprolog-rh/doc/manual024.html
 *
 */

 /* How to declare predicate types:
  * type(Predicate_Name, Type).
  * where type is a list of the types of the Terms in the predicate.
  */

/* Predicate: type(Term, Type)
 * Meaning: Term "Term" has type "Type".
 */

 /* Booleans */
 % Ture -> tru.
 % False -> fls.

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

/* Start with derivable base types. */
% TODO: Look into why this works, but only on first check.
type(0, nat).
type(X, nat) :- X > 0, Y is X-1, type(Y, nat). % Downward recursion.
type(X, nat) :- X < 0, Y is X+1, type(Y, nat). % Upward recursion.
% Math types.
% TODO: This needs updating to not just always be valid.
type(add(_,_,_),[nat,nat,nat]).
type(sub(_,_,_),[nat,nat,nat]).
type(mul(_,_,_),[nat,nat,nat]).
type(div(_,_,_),[nat,nat,nat]).
% Boolean types.
type(tru, bool).
type(fls, bool).
type(ifthenelse(A,B,C,D),[bool,T2,T2,T2]) :-
	type(A, bool),
	type(B, T2),
	type(C, T2),
	type(D, T2).
