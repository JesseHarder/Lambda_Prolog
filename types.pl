/*
 * Prolog code to be used to write type-checkable prolog code.
 *
 * Note, existing type checking predicates in prolog are listed here:
 * http://pauillac.inria.fr/~haemmerl/gprolog-rh/doc/manual024.html
 *
 */

:- [numbers, booleans].

 /* Listing Valid Types */
 type('Unit').
 type('Bool').
 type(number).

 /* ----- Atom and Variable Types ----- */
 % Unit type
 type(unit, 'Unit').
 % Booleans
 type(tru, 'Bool').
 type(fls, 'Bool').
 % Numbers - Anything instatiated to a number has type "number".
 type(X, number) :- number(X).
 /* Variables - can be of any type. */
 type(Var, Type) :- var(Var), type(Type).

 /* ----- Predicate Types ----- */
 /* -- Booleans -- */
 % ifthenelse: 'Bool' -> T -> T -> T
 type(ifthenelse(A,B,C,D),['Bool',T2,T2,T2]) :-
    ifthenelse(A,B,C,D),
    type(A, 'Bool'),
  	type(B, T2),
  	type(C, T2),
  	type(D, T2).
 % and: 'Bool' -> 'Bool' -> 'Bool'
 type(and(X,Y,Z),['Bool','Bool','Bool']) :-
    and(X,Y,Z),
    type(X, 'Bool'),
 	type(Y, 'Bool'),
 	type(Z, 'Bool').
 % or: 'Bool' -> 'Bool' -> 'Bool'
 type(or(X,Y,Z),['Bool','Bool','Bool']) :-
    or(X,Y,Z),
 	type(X, 'Bool'),
 	type(Y, 'Bool'),
 	type(Z, 'Bool').
 % xor: 'Bool' -> 'Bool' -> 'Bool'
 type(xor(X,Y,Z),['Bool','Bool','Bool']) :-
    xor(X,Y,Z),
 	type(X, 'Bool'),
 	type(Y, 'Bool'),
 	type(Z, 'Bool').
 % not: 'Bool' -> 'Bool'
 type(not(X,Y),['Bool','Bool']) :-
    not(X,Y),
 	type(X, 'Bool'),
 	type(Y, 'Bool').

/* -- Numbers -- */
% succ: number -> number
type(succ(X,Y),[number,number]) :-
    succ(X,Y),
	type(X, number),
	type(Y, number).
% pred: number -> number
type(pred(X,Y),[number,number]) :-
    pred(X,Y),
	type(X, number),
	type(Y, number).
% pred: number -> 'Bool'
type(iszero(X,Y),[number,'Bool']) :-
    iszero(X,Y),
    type(X, number),
	type(Y, 'Bool').
% add: number -> number -> number
type(add(X,Y,S),[number,number,number]) :-
    add(X,Y,S),
    type(X, number),
	type(Y, number),
    type(S, number).
% sub: number -> number -> number
type(sub(X,Y,D),[number,number,number]) :-
    sub(X,Y,D),
    type(X, number),
	type(Y, number),
    type(D, number).
% mul: number -> number -> number
type(mul(X,Y,P),[number,number,number]) :-
    mul(X,Y,P),
    type(X, number),
	type(Y, number),
    type(P, number).
% div: number -> number -> number
type(div(N,D,Q),[number,number,number]) :-
    div(N,D,Q),
    type(N, number),
	type(D, number),
    type(Q, number).
