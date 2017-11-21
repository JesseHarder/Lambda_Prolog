/* This file contains all of the lambda calculus evaluation rules as
 * implemented in Prolog. The predicate evaluate/2 is used for these
 * rules. They take the form:
 * eval(X,Y)
 *		where Y is the result of evaulating X.
 */

:- [values,
	lambda/lambdas,
	util/plists].

/* --- Helper Predicates --- */

% eval_if_not_value/1
% If the term to be evaluated is already a value, the result is the term.
% If the term to be evaluated is not a value, the result is the evaluation of
%	the term.
eval_if_not_value(Term,Result) :-
	(is_value(Term) ->
		% If Term is a value, it is the result.
		Result = Term;
		% If not, the result of evaluating it is the result.
		eval(Term,Result)).

/* --- Booelean Evaluation --- */
%E-IfTrue
eval(ifte(tru,Term1,_),Result) :- eval_if_not_value(Term1,Result).
%E-IfFalse
eval(ifte(fls,_,Term2),Result) :- eval_if_not_value(Term2,Result).
%E-If
eval(ifte(Term1, Term2, Term3),Result) :-
	eval(Term1, New1),
	eval_if_not_value(ifte(New1, Term2, Term3),Result).
	% eval(ifte(New1, Term2, Term3),Result).

/* --- Natural Number Evaluation --- */
% E-PredZero
eval(pred(0),0) :- !.
% E-PredSucc
eval(pred(succ(X)),X) :-
	is_natural_value(X),!.
% E-Succ
eval(succ(Term),Result) :-
	eval(Term,NewTerm),
	eval_if_not_value(succ(NewTerm),Result),!.
% E-Pred
eval(pred(Term),Result) :-
	eval(Term,NewTerm),
	eval_if_not_value(pred(NewTerm),Result),!.
% E-IsZeroZero
eval(iszero(0),tru).
% E-IsZeroSucc
eval(iszero(succ(X)),fls) :- is_natural_value(X).
% E-IsZero
eval(iszero(Term),Result) :-
	eval(Term,NewTerm),
	eval(iszero(NewTerm),Result).

/* --- Tuples --- */
% E-ProjTuple
eval(proj(tuple(List),Index),Result) :-
	is_value(tuple(List)),
	is_list(List), length(List,Len), Index >= 1, Index =< Len, % Sanity Check
	ith_elm(Index,List,Result).
% E-Proj
eval(proj(tuple(List),Index), Result) :-
	eval(tuple(List), NewTuple),
	eval(proj(NewTuple,Index), Result).
% E-Tuple - This is sort of the Big-Step version of this.
eval(tuple(List), tuple(Vals)) :-
	is_not_value(tuple(List)),
	maplist(eval_if_not_value,List,Vals).

/* --- Records --- */
% E-ProjRecord
eval(proj(record(List),Label),Result) :-
	is_value(record(List)),
	is_list(List), string(Label), % Sanity Check
	member(Label=Result,List).
% E-Proj
% E-Rcd

/* --- Basic Lambda Calculus Evaluation --- */
% E-APP1
eval([Term1,Term2], Result) :-
	is_not_value(Term1),
	is_not_value(Term2),
	eval(Term1,New1),
	eval([New1,Term2], Result).

% E-APP2
eval([Val,Term], Result) :-
	is_value(Val),
	is_not_value(Term),
	eval(Term,NewTerm),
	eval([Val,NewTerm], Result).

% E-AppAbs
eval([Abs,Val],Result) :-
	is_lambda(Abs),
	is_value(Val),
	apply(Abs,Val,ApResult),
	eval_if_not_value(ApResult,Result),!.

eval([Abs,Val|OtherTerms],Result) :-
	length([Abs,Val|OtherTerms],Len), Len > 2,
	eval([Abs,Val],NewTerm),
	eval([NewTerm|OtherTerms],MidResult),
	eval_if_not_value(MidResult,Result),!.
