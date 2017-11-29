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

/* ---------- typeof/1 - Listing Valid Types ----------
 * This section is like the "T::=..." section of our syntax.
 */

type('Bool').
type('Natural').
% Function Type:
%   (T1 -> T2) is T1 -> T2.
%   ((T1 -> T2) -> T3) is (T1 -> T2) -> T3.
%   (T1 -> (T2 -> T3)) is T1 -> (T2 -> T3).
type((T1->T2)) :- type(T1),type(T2).


/* ---------- typeof/2 ---------- */
/* This is now used only to kickstart the process, allowing the user to note
 * need to explicitly write the ([], at the start of typeof/3.
 */
typeof(Term, Type) :- typeof([], Term, Type).


/* ---------- typeof/3 - The Typing Rules ---------- */

/***** Unbound Variables *****/
% T-VarProlog
%   It suffices to allow variables to be resolved to any type
%   that will unify with all other typing restrictions.
% IMPORTANT: The placement of this rule above all other typeof/3
%   rules is key to preventing infinite loops when guessing variable types.
typeof(Env, Var, Type) :-
 var(Var),
 member(Var:Type, Env).

/***** Booleans *****/
typeof(_,tru, 'Bool'). % T-True
typeof(_,fls, 'Bool'). % T-False
% T-If
typeof(Env, ifte(Term1,Term2,Term3), Type) :-
    typeof(Env,Term1, 'Bool'),
    typeof(Env,Term2,Type),
    typeof(Env,Term3,Type).

/***** Numbers *****/
typeof(_, 0, 'Natural').
typeof(Env, succ(X), 'Natural') :- typeof(Env, X, 'Natural'). % T-Succ
typeof(Env, pred(X), 'Natural') :- typeof(Env, X, 'Natural'). % T-Succ
typeof(Env, iszero(X), 'Bool') :- typeof(Env, X, 'Natural'). % T-IsZero

/***** Abstraction *****/
% T-AbsProlog
%   In Prolog, It suffices to just say that the type of a lambda is
%   VarType -> SubtermType, where VarType is the type of the variable and
%   SubtermType is the type of the subterm. Prolog unification will force a
%   type environment in which this is always true.
typeof(Env, lam(Var:VarType,Subterm), Type) :-
    is_list(Subterm),   % Sanity Check
    NewEnv = [Var:VarType|Env],
    (Subterm = [InnerTerm] ->           % If the lambda has just one subterm,
        typeof(NewEnv,InnerTerm,SubtermType);  % its type the type of the subterm.
        typeof(NewEnv,Subterm,SubtermType)),   % Else, the subterm type is that of the application.
    Type = (VarType->SubtermType), !.

% T-AppBase
%   An application returns the return type of the first term, which should be
%   an abstraction, if the second term has the abstractions parameter type.
typeof(Env, [lam(X:ParamType,List),Term2],ReturnType) :-
    % First line needed to prevent infinite option search for Term1.
    % TODO: Possibly cheating? Consider alternate methods. Works for now.
    typeof(Env, lam(X:ParamType,List), (ParamType->ReturnType)),
    typeof(Env, Term2,ParamType), !.
% T-AppRecurse
typeof(Env, List, Type) :-
    is_list(List), length(List, Len), Len > 2,
    list_layer_left(List, LayeredList),
    typeof(Env, LayeredList, Type).
