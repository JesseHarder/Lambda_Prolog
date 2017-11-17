/* This file is for tracking the valid form of a value in Prolog Lambda.
 * It does so through the predicate is_value(X).
 */

:- [lambda/lambdas].

% Temporary. Remove eventually.
is_value(X) :- atom(X).
% Abstractions are values.
is_value(X) :- is_lambda(X).
/* Booleans */
is_value(tru).
is_value(fls).
