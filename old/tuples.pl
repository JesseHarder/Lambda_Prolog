/* Predicates for working with Tuples.
 * For now, Tuples are just Lists with tuple() wrapped around them.
 */

:- [plists].

/* Predicate for getting the ith element of a tuple, via projection. */
% Just get the ith element of the tuple's list.
project(tuple(TList),Index,Elm) :-
	is_list(TList),
	ith_elm(TList, Index, Elm).
