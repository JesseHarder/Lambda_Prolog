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

type('Bool').
type('Natural').
% Function Type:
%   [T1, T2] is T1 -> T2.
%   [[T1, T2], T3] is (T1 -> T2) -> T3.
%   [T1, [T2,T3]] is T1 -> (T2 -> T3).
type([T1,T2]) :- type(T1),type(T2).

/* ---------- Typing Rules ---------- */

/***** Booleans *****/
typeof(tru, 'Bool'). % T-True
typeof(fls, 'Bool'). % T-False
% T-If
typeof(ifte(Term1,Term2,Term3), Type) :-
    typeof(Term1, 'Bool'),
    typeof(Term2,Type),
    typeof(Term3,Type).

/***** Numbers *****/
typeof(0, 'Natural').
typeof(succ(X), 'Natural') :- typeof(X, 'Natural'). % T-Succ
typeof(pred(X), 'Natural') :- typeof(X, 'Natural'). % T-Succ
typeof(iszero(X), 'Bool') :- typeof(X, 'Natural'). % T-IsZero

/***** Variables *****
 * In Prolog, the type environment is constructed by the unification
 * of all typing information. The following rules work based on that.
 */

% T-AbsProlog
%   In Prolog, It suffices to just say that the type of a lambda is
%   VarType -> SubtermType, where VarType is the type of the variable and
%   SubtermType is the type of the subterm. Prolog unification will force a
%   type environment in which this is always true.
typeof(lam(Var:VarType,Subterm),Type) :-
    is_list(Subterm),   % Sanity Check
    typeof(Var,VarType),
    (Subterm = [InnerTerm] ->           % If the lambda has just one subterm,
        typeof(InnerTerm,SubtermType);  % its type the type of the subterm.
        typeof(Subterm,SubtermType)),   % Else, the subterm type is that of the application.
    Type = [VarType,SubtermType], !.

% T-AppProlog
%   An application returns the return type of the first term, which should be
%   an abstraction, if the second term has the abstractions parameter type.
typeof([lam(X:ParamType,List),Term2],ReturnType) :-
    % First line needed to prevent infinite option search for Term1.
    % TODO: Possibly cheating? Consider alternate methods. Works for now.
    var(X), X = Term2,
    typeof(Term2,ParamType),
    typeof(lam(X:ParamType,List),[ParamType,ReturnType]),!.
% TODO: Recursive version:
typeof(List, Type) :-
    is_list(List), length(List, Len), Len > 2,
    list_layer_left(List, LayeredList),
    typeof(LayeredList, Type).

typeof(_, Term, Type) :- typeof(Term, Type).

/***** Unbound Variables *****/
% T-VarProlog
%   It suffices to allow variables to be resolved to any type
%   that will unify with all other typing restrictions.
typeof(Env, Var, Type) :-
    var(Var),
    member(Var:Type, Env).
