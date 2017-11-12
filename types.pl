/*
 * Prolog code to be used to write type-checkable prolog code.
 *
 * Note, existing type checking predicates in prolog are listed here:
 * http://pauillac.inria.fr/~haemmerl/gprolog-rh/doc/manual024.html
 *
 */

:- [numbers, booleans].

 /* Listing Valid Types */
 type(bool).
 type(number).

 /* ----- Atom and Variable Types ----- */
 % Booleans
 type(tru, bool).
 type(fls, bool).
 % Numbers - Anything instatiated to a number has type "number".
 type(X, number) :- number(X).
 /* Variables - can be of any type. */
 type(Var, Type) :- var(Var), type(Type).

 /* ----- Predicate Types ----- */
 /* -- Booleans -- */
 % ifthenelse: bool -> T -> T -> T
 type(ifthenelse(A,B,C,D),[bool,T2,T2,T2]) :-
    ifthenelse(A,B,C,D),
    type(A, bool),
  	type(B, T2),
  	type(C, T2),
  	type(D, T2).
 % and: bool -> bool -> bool
 type(and(X,Y,Z),[bool,bool,bool]) :-
    and(X,Y,Z),
    type(X, bool),
 	type(Y, bool),
 	type(Z, bool).
 % or: bool -> bool -> bool
 type(or(X,Y,Z),[bool,bool,bool]) :-
    or(X,Y,Z),
 	type(X, bool),
 	type(Y, bool),
 	type(Z, bool).
 % xor: bool -> bool -> bool
 type(xor(X,Y,Z),[bool,bool,bool]) :-
    xor(X,Y,Z),
 	type(X, bool),
 	type(Y, bool),
 	type(Z, bool).
 % not: bool -> bool
 type(not(X,Y),[bool,bool]) :-
    not(X,Y),
 	type(X, bool),
 	type(Y, bool).

/* -- Numbers -- */

/* TODO: Figure out why the following four run forever if asked to check
 * for a second result
 */
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
% pred: number -> bool
type(iszero(X,Y),[number,bool]) :-
    iszero(X,Y),
    type(X, number),
	type(Y, bool).
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
