/*
 * Prolog code to be used to write type-checkable prolog code.
 *
 * Note, existing type checking predicates in prolog are listed here:
 * http://pauillac.inria.fr/~haemmerl/gprolog-rh/doc/manual024.html
 *
 */

 /* Included Labda calculus features:
  *     Booleans
  *     Numbers
  *     Unit
  *
  * Derived featurs:
  *     Let - this is basically just Prolog's = operator. If I write something
  *         like X = A+B*C, X is not the same thing as A+B*C, unevevaluated.
  *     Ascription - this is basically just adding a note of a new type. See
  *         the Ascription comment below for where these should go and how
  *         they should look.
  */

:- [numbers, booleans].

 /* Listing Valid Types */
 type('Unit').
 type('Bool').
 type('Number').
 % Function Type: where [T1, T2, T3] is T1 -> T2 -> T3.
 type([T]) :- type(T).
 type([Head|Tail]) :- type(Head),type(Tail).

 /* ----- Atom and Variable Types ----- */
 % Unit type
 type(unit, 'Unit').
 % Booleans
 type(tru, 'Bool').
 type(fls, 'Bool').
 % Numbers - Anything instatiated to a number has type 'Number'.
 type(X, 'Number') :- number(X).

/* Ascriptions - Add ascription below this comment of the form:
 *      type(X, <NewTypeName>) :- type(X, <OldTypeRepresentation>).
 *  Example:
 *      type(X, 'NNN') :- type(X, ['Number','Number','Number']).
 */

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
% succ: 'Number' -> 'Number'
type(succ(X,Y),['Number','Number']) :-
    succ(X,Y),
	type(X, 'Number'),
	type(Y, 'Number').
% pred: 'Number' -> 'Number'
type(pred(X,Y),['Number','Number']) :-
    pred(X,Y),
	type(X, 'Number'),
	type(Y, 'Number').
% pred: 'Number' -> 'Bool'
type(iszero(X,Y),['Number','Bool']) :-
    iszero(X,Y),
    type(X, 'Number'),
	type(Y, 'Bool').
% add: 'Number' -> 'Number' -> 'Number'
type(add(X,Y,S),['Number','Number','Number']) :-
    add(X,Y,S),
    type(X, 'Number'),
	type(Y, 'Number'),
    type(S, 'Number').
% sub: 'Number' -> 'Number' -> 'Number'
type(sub(X,Y,D),['Number','Number','Number']) :-
    sub(X,Y,D),
    type(X, 'Number'),
	type(Y, 'Number'),
    type(D, 'Number').
% mul: 'Number' -> 'Number' -> 'Number'
type(mul(X,Y,P),['Number','Number','Number']) :-
    mul(X,Y,P),
    type(X, 'Number'),
	type(Y, 'Number'),
    type(P, 'Number').
% div: 'Number' -> 'Number' -> 'Number'
type(div(N,D,Q),['Number','Number','Number']) :-
    div(N,D,Q),
    type(N, 'Number'),
	type(D, 'Number'),
    type(Q, 'Number').
