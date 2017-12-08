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
    lambda/rec_var].

/* ---------- type/1 - Listing Valid Types ----------
 * This section is like the "T::=..." section of our syntax.
 */

% Unit Type
type('Unit').
% Booleans
type('Bool').
% Natural Numbers
type('Natural').
% Tuples
type('Tuple'([])).
type('Tuple'([H|T])) :-
    type(H),
    type('Tuple'(T)).
% Records
type('Record'([])).
type('Record'([Label=Type|Tail])) :-
    string(Label), type(Type),
    type('Record'(Tail)).
% Variants
type('Variant'([Label=Type])) :- string(Label), type(Type).
type('Variant'([Label=Type|Tail])) :-
    type('Variant'([Label=Type])),
    type('Variant'(Tail)).
% Lists
type('List'(T)) :- type(T).
% Function Type:
%   (T1 -> T2) is T1 -> T2.
%   ((T1 -> T2) -> T3) is (T1 -> T2) -> T3.
%   (T1 -> (T2 -> T3)) is T1 -> (T2 -> T3).
type((T1->T2)) :- type(T1),type(T2).

% Denoting what type rasie exceptions will pass.
type_exn('Natural').

/* ---------- typeof/2 ---------- */
/* This is now used only to kickstart the process, allowing the user to note
 * need to explicitly write the ([], at the start of typeof/3.
 */
typeof(Term, Type) :- typeof([], Term, Type).


/* ---------- typeof/3 - The Typing Rules ---------- */

/***** Unbound Variables *****/
% T-Var
% IMPORTANT: The placement of this rule above all other typeof/3
%   rules is key to preventing infinite loops when guessing variable types.
typeof(Env, Var, Type) :-
    atom(Var),
    member(Var:Type, Env),!.

/***** Unit type *****/
typeof(_, unit, 'Unit') :- !. % T-Unit

/***** Booleans *****/
% T-True
typeof(_, tru, 'Bool') :- !.
% T-False
typeof(_, fls, 'Bool') :- !.
% T-If
typeof(Env, ifte(Term1, Term2, Term3), Type) :-
    typeof(Env, Term1, 'Bool'),
    typeof(Env, Term2, Type),
    typeof(Env, Term3, Type),!.

/***** Numbers *****/
 % T-Zero
typeof(_, 0, 'Natural') :- !.
% T-Succ
typeof(Env, succ(X), 'Natural') :-
    typeof(Env, X, 'Natural'),!.
% T-Pred
typeof(Env, pred(X), 'Natural') :-
    typeof(Env, X, 'Natural'),!.
% T-IsZero
typeof(Env, iszero(X), 'Bool') :-
    typeof(Env, X, 'Natural'),!.

/***** Abstraction *****/
% T-Abs
typeof(Env, lam(Var:VarType, Subterm), Type) :-
    NewEnv = [Var:VarType|Env],
    typeof(NewEnv, Subterm, SubtermType),
    Type = (VarType->SubtermType), !.

% T-AppBase
%   An application returns the return type of the first term, which should be
%   an abstraction, if the second term has the abstractions parameter type.
typeof(Env, [Term1,Term2],ReturnType) :-
    typeof(Env, Term1, (ParamType->ReturnType)),
    typeof(Env, Term2, ParamType), !.
% T-AppRecurse
typeof(Env, List, Type) :-
    is_list(List), length(List, Len), Len > 2,
    list_layer_left(List, LayeredList),
    typeof(Env, LayeredList, Type), !.

/***** Let *****/
% T-Let
typeof(Env, let(X=Term1, Term2), Type2) :-
    typeof(Env, Term1, Type1),
    NewEnv = [X:Type1|Env],
    typeof(NewEnv, Term2, Type2),!.

/***** Tuples *****/
% T-Tuple
% The Types list in 'Tuple'() is the corresponding list of calling
%   typeof on each of the elements in the List inside of tuple().
%   mep_typeof does exactly that.
typeof(Env, tuple(List), 'Tuple'(Types)) :-
    is_list(List), length(List, L), L >= 0, % "Lists" is a non-empty list.
    map_typeof(Env,List,Types),!.
% T-ProjTupl
typeof(Env, proj(tuple(List), Index), Type) :-
    typeof(Env, tuple(List), 'Tuple'(_)),
    ith_elm(Index, List, Elm),
    typeof(Env, Elm, Type),!.

/***** Records *****/
% T-Record
% The Types list in 'Record'() is the corresponding list of calling
%   typeof on each of the elements in the List inside of record().
%   map_typeof does exactly that.
typeof(Env, record(List), 'Record'(Types)) :-
    is_list(List), length(List, L), L >= 0, % "Lists" is a non-empty list.
    record_parts(record(List), Labels, Vals),
    map_typeof(Env,Vals,ValTypes),
    record_parts(record(Types), Labels, ValTypes),!.
% T-ProjRcd
typeof(Env, proj(record(List), Label), Type) :-
    typeof(Env, record(List), 'Record'(_)),
    member(Label=Term, List),
    typeof(Env, Term, Type),!.

/***** Variants *****
 * NOTE: T-Vairant might cause problems if used incorrectly.
 * This is because it could potentially generate infinite different type
 * results for a vairant term.
 */

% T-Variant
typeof(Env, var(Label=Term), 'Variant'(VariantList)) :-
    typeof(Env, Term, TermType),
    member(Label=TermType, VariantList).
% T-Case - NOTE: This is more what was needed for Prolog to work to get the
%        correct behavior rather than a direct translation.
typeof(Env, case(var(Label=Term), Conditions), Type) :-
    is_list(Conditions),
    member(var(CondLabel=CondVar)->CondTerm, Conditions),
	Label=CondLabel,
	CondVar=Term,
	typeof(Env, CondTerm, Type),!.



/***** Lists *****/
% TODO: Check with Cormac about why Lists needed explicit typing in the book.
% T-Nil
typeof(_, nil,'List'(T)) :- type(T),!. % Empty list can be list of any type.
% T-Cons
typeof(Env, cons(Head, Tail), 'List'(HType)) :-
    typeof(Env, Head, HType),
    typeof(Env, Tail, 'List'(HType)),!.
% T-IsNil
typeof(Env, isnil(Term), 'Bool') :-
    typeof(Env, Term, 'List'(_)),!.
% T-Head
typeof(Env, head(Term), Type) :-
    typeof(Env, Term, 'List'(Type)),!.
% T-Tail
typeof(Env, tail(Term), 'List'(Type)) :-
    typeof(Env, Term, 'List'(Type)),!.

/***** Exceptions *****/
% -- The following is completmented out because you can't have it and
%       the raise() version at the same time.
% T-Error - An error can be of any type.
% typeof(_, error, Type) :- type(Type),!.
% % T-Try (Error)
% typeof(Env, try(Term1, Term2), Type) :-
%     typeof(Env, Term2, Type),    % Best to check T2's type first,
%     typeof(Env, Term1, Type),!.    % as T1 might be error.
% T-Raise
typeof(Env, raise(Term1), Type) :-
    type_exn(T_Exn), typeof(Env, Term1, T_Exn),
    type(Type),!.
% T-Try (Rasie)
typeof(Env, try(Term1, Term2), Type) :-
    type_exn(T_Exn),
    typeof(Env, Term2, (T_Exn->Type)),    % Best to check T2's type first,
    typeof(Env, Term1, Type),!.               % as T1 might be raise.


/* ---------- Helper Functions ---------- */
map_typeof(Env, Vals, Types) :-
    length(Vals, Length),
    env_list_len(Env, EnvList, Length),
    maplist(typeof, EnvList, Vals,Types),!.
