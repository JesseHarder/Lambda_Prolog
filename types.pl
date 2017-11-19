/* Lambda calculuc implemented in prolog.
 *
 * Note, existing type checking predicates in prolog are listed here:
 * http://pauillac.inria.fr/~haemmerl/gprolog-rh/doc/manual024.html
 *
 */

 /* Included Labda calculus features:
  *     Booleans
  *     Numbers
  */

:- [util/plists].

/* Helper Rules */
types_in_list([Head], [Type]) :- type(Head,Type).
types_in_list([Head|Tail], [T_Head|T_Tail]) :-
    type(Head, T_Head),
    types_in_list(Tail, T_Tail).

/* ---------- Listing Valid Types ----------
 * This section is like the "T::=..." section of our syntax.
 */

% type('Unit').
type('Bool').
type('Natural').
% type('List'(T)) :- type(T).
% type('Tuple'([H])) :- type(H).
% type('Tuple'([H|T])) :- type(H), type('Tuple'(T)).
% Function Type: where [T1, T2, T3] is T1 -> T2 -> T3.
type([T]) :- type(T).
type([H|T]) :- type(H),type(T).





/* ---------- Typing Rules ---------- */
/***** Unit type *****/
% typeof(unit, 'Unit').

/***** Booleans *****/
typeof(tru, 'Bool'). % T-True
typeof(fls, 'Bool'). % T-False
% T-If
typeof(ifte(Term1,Term2,Term3), Type) :-
    typeof(Term1, 'Bool'),
    typeof(Term2,Type),
    typeof(Term3,Type).

/***** Numbers *****/
typeof(0, 'Natural').   % T-Zero
typeof(succ(X), 'Natural') :- typeof(X, 'Natural'). % T-Succ
typeof(pred(X), 'Natural') :- typeof(X, 'Natural'). % T-Succ
typeof(iszero(X), 'Bool') :- typeof(X, 'Natural'). % T-IsZero

/***** Let *****/
typeof(let(X,Term1,Term2), Type2) :-
    var(X), X=Term1,
    typeof(Term2, Type2).

/***** Lists *****/
% typeof([],'List'(T)) :- type(T). % Empty list can be list of any type.
% typeof([Head|Tail],'List'(T)) :-   % A list has type list of T's if
%      typeof(Head, T),             % the fisrt element has type T and
%      typeof(Tail, 'List'(T)).       % the tail is a list of T's.

/***** Tuples *****/
% typeof(tuple([Val]), 'Tuple'([T])) :- typeof(Val,T).
% typeof(tuple(List), 'Tuple'(Types)) :-
%     is_list(List), length(List, L), L > 0, % "Lists" is a non-empty list.
%     types_in_list(List,Types).

/***** Variables *****
 * Variables can be of any type.
 */
typeof(Var, Type) :- var(Var), type(Type).

/* Ascriptions - Add ascription below this comment of the form:
 *      typeof(X, <NewTypeName>) :- typeof(X, <OldTypeRepresentation>).
 *  Example:
 *      typeof(X, 'NNN') :- typeof(X, ['Natural','Natural','Natural']).
 */
