/* This file is for tracking the valid form of a value in Prolog Lambda.
 * It does so through the predicate is_value(X).
 */

:- [lambda/lambdas].

/* --- Healper Predicates --- */
% Natural Numbers
is_natural_value(0).
is_natural_value(succ(X)) :- is_natural_value(X).

/* --- is_value/1 --- */
% Temporary. Remove eventually.
is_value(X) :- atom(X).
% Abstractions are values.
is_value(X) :- is_lambda(X).
% Booleans - tru and fls are values.
is_value(tru).
is_value(fls).
% Natural numbers are values.
is_value(X) :- is_natural_value(X).
