/* Predicates for working with Tuples.
 * For now, Tuples are just Lists with tuple() wrapped around them.
 */

/* Predicate for getting the ith element of a tuple. */
% First form is for getting first element.
ith_elm(tuple([Head|_]),1,Elm) :-
	Elm = Head.
% Recursive form for getting ith element.
ith_elm(tuple([_|Tail]),I,Elm) :-
	I > 1,
	J is I-1,
	ith_elm(tuple(Tail),J,Elm).
