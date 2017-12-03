/* This file is for tracking the valid form of a value in Prolog Lambda.
 * It does so through the predicate is_value(X).
 *
 * This section of the code corresponds to the v::=... section of the
 * semantic rules.
 */

:- [lambda/lambdas].

/* --- Healper Predicates --- */
% Natural Numbers
is_natural_value(0).
is_natural_value(succ(X)) :- is_natural_value(X).
% Is Not Value.
is_not_value(X) :- not(is_value(X)).

/* --- is_value/1 --- */
% Abstractions are values.
is_value(X) :- is_lambda(X).
% Booleans - tru and fls are values.
is_value(tru).
is_value(fls).
% Natural numbers are values.
is_value(X) :- is_natural_value(X).
% Unit
is_value(unit).
% Tuples - A tuple is a value if every item in it is a value.
is_value(tuple(List)) :- forall(member(Val,List), is_value(Val)).
% Records - A record is a value if every item in it is a value
%	(and labels are strings).
is_value(record(List)) :-
	forall(member(_=Val,List), is_value(Val)),
	forall(member(Label=_,List), string(Label)).
% Lists
is_value(nil).
is_value(cons(V1, V2)) :-
	is_value(V1),
	is_value(V2).
