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

:- [util/plists,
    lambda/records].

/* ---------- Listing Valid Types ----------
 * This section is like the "T::=..." section of our syntax.
 */

% type('Unit').
type('Bool').
type('Natural').
% type('List'(T)) :- type(T).
type('Tuple'([H])) :- type(H).
type('Tuple'([H|T])) :-
    type(H),
    type('Tuple'(T)).
type('Record'([Label=Type])) :- string(Label), type(Type).
type('Record'([Label=Type|Tail])) :-
    type('Record'([Label=Type])),
    type('Record'(Tail)).
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

/***** Lists *****/
% typeof([],'List'(T)) :- type(T). % Empty list can be list of any type.
% typeof([Head|Tail],'List'(T)) :-   % A list has type list of T's if
%      typeof(Head, T),             % the fisrt element has type T and
%      typeof(Tail, 'List'(T)).       % the tail is a list of T's.

/***** Tuples *****/
% T-Tuple
% The Types list in 'Tuple'() is the corresponding list of calling
%   typeof on each of the elements in the List inside of tuple().
%   maplist does exactly that.
typeof(tuple(List), 'Tuple'(Types)) :-
    is_list(List), length(List, L), L > 0, % "Lists" is a non-empty list.
    maplist(typeof,List,Types).
% T-ProjTupl
typeof(proj(tuple(List), Index), Type) :-
    typeof(tuple(List), 'Tuple'(_)),
    ith_elm(Index, List, Elm),
    typeof(Elm, Type).

/***** Records *****/
% T-Records
% The Types list in 'Tuple'() is the corresponding list of calling
%   typeof on each of the elements in the List inside of tuple().
%   maplist does exactly that.
typeof(record(List), 'Record'(Types)) :-
    is_list(List), length(List, L), L > 0, % "Lists" is a non-empty list.
    record_parts(record(List), Labels, Vals),
    maplist(typeof,Vals,ValTypes),
    record_parts(record(Types), Labels, ValTypes).
% T-ProjRcd
typeof(proj(record(List), Label), Type) :-
    typeof(record(List), 'Record'(_)),
    member(Label=Term, List),
    typeof(Term, Type).

/***** Variables *****
 * Variables can be of any type.
 */
typeof(Var, Type) :- var(Var), type(Type).

/* Ascriptions - Add ascription below this comment of the form:
 *      typeof(X, <NewTypeName>) :- typeof(X, <OldTypeRepresentation>).
 *  Example:
 *      typeof(X, 'NNN') :- typeof(X, ['Natural','Natural','Natural']).
 */
