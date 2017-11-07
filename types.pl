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
% sub(X,Y,D) :- D is X-Y.
% mult(X,Y,P) :- P is X*Y.

/* ^^^^^ KNOWLEDGE BASE ABOVE ^^^^^*/
/* VVVVVVVVV TYPING BELOW VVVVVVVVV*/

/* Start with derivable base types. */
% type(X, int) :- integer(X).
% type(X, float) :- float(X).
type(X, number) :- number(X).
% Math predicate types.
type(add(_,_,_),[number,number,number]).
% Boolean types.
type(tru, bool).
type(fls, bool).
type(ifthenelse(A,B,C,_),[T1,T2,T2,T2]) :-
	type(A, T1),
	type(B, T2),
	type(C, T2).
