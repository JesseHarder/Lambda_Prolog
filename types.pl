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
  *     Tuples (built on top of lists.)
  *     Lists (using built in list type.)
  *
  * Derived featurs:
  *     Application! - Turns out this is already in Prolog. See application.txt
  *         for a more detailed description.
  *     Let - this is basically just Prolog's = operator. If I write something
  *         like X = A+B*C, X is not the same thing as A+B*C, unevevaluated.
  *     Ascription - this is basically just adding a note of a new type. See
  *         the Ascription comment below for where these should go and how
  *         they should look.
  *     Sums and Variants - Already built in? Just write same predicate to
  *         work for different type inputs?
  *     Fix - Not needed? Prolog is already recursive.
  */

:- [util/plists].

/* Helper Rules */
types_in_list([Head], [Type]) :- type(Head,Type).
types_in_list([Head|Tail], [T_Head|T_Tail]) :-
    type(Head, T_Head),
    types_in_list(Tail, T_Tail).

/* Listing Valid Types */
type('Unit').
type('Bool').
type('Number').
type('List'(T)) :- type(T).
type('Tuple'([H])) :- type(H).
type('Tuple'([H|T])) :- type(H), type('Tuple'(T)).
% Function Type: where [T1, T2, T3] is T1 -> T2 -> T3.
type([T]) :- type(T).
type([H|T]) :- type(H),type(T).




/* ----- Non-Predicate Types ----- */
% Unit type
typeof(unit, 'Unit').
% Booleans
typeof(tru, 'Bool'). % T-True
typeof(fls, 'Bool'). % T-False
% Numbers - Anything instatiated to a number has type 'Number'.
typeof(X, 'Number') :- number(X).
% Lists
typeof([],'List'(T)) :- type(T). % Empty list can be list of any type.
typeof([Head|Tail],'List'(T)) :-   % A list has type list of T's if
     typeof(Head, T),             % the fisrt element has type T and
     typeof(Tail, 'List'(T)).       % the tail is a list of T's.
% Tuples
typeof(tuple([Val]), 'Tuple'([T])) :- typeof(Val,T).
typeof(tuple(List), 'Tuple'(Types)) :-
    is_list(List), length(List, L), L > 0, % "Lists" is a non-empty list.
    types_in_list(List,Types).

/* Ascriptions - Add ascription below this comment of the form:
 *      typeof(X, <NewTypeName>) :- typeof(X, <OldTypeRepresentation>).
 *  Example:
 *      typeof(X, 'NNN') :- typeof(X, ['Number','Number','Number']).
 */

/* Variables - can be of any type. */
typeof(Var, Type) :- var(Var), type(Type).

/* ----- Predicate Types ----- */
/* --- Booleans --- */
% T-If
typeof(ifte(Term1,Term2,Term3), Type) :-
    typeof(Term1, 'Bool'),
    typeof(Term2,Type),
    typeof(Term3,Type).
