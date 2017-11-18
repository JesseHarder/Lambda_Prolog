/* This file contains all of the lambda calculus evaluation rules as
 * implemented in Prolog. The predicate evaluate/2 is used for these
 * rules. They take the form:
 * eval(X,Y)
 *		where Y is the result of evaulating X.
 */

:- [values,
	lambda/lambdas].

/* --- Helper Predicates --- */

% eval_if_not_value/1
% If the term to be evaluated is already a value, the result is the term.
% If the term to be evaluated is not a value, the result is the evaluation of
%	the term.
eval_if_not_value(Term,Result) :-
	(is_value(Term) ->
		Result = Term;
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

% eval(succ(Term),Result) :-
% 	eval(Term,NewTerm),
% 	is_value(succ(NewTerm)),
% 	Result = succ(NewTerm),!.
%
% eval(succ(Term),Result) :-
% 	eval(Term,NewTerm),
% 	eval(succ(NewTerm),Result),!.
/* --- Basic Lambda Calculus Evaluation --- */
% E-AppAbs
eval([Abs,Val],Result) :-
	is_lambda(Abs),
	is_value(Val),
	apply(Abs,Val,Result),
	% If app result is a value, you're done.
	is_value(Result).

eval([Abs,Val],Result) :-
	is_lambda(Abs),
	is_value(Val),
	apply(Abs,Val,MidResult),
	% If app result not a value, continue eval.
	eval(MidResult,Result).

eval([Abs,Val|OtherTerms],Result) :-
	is_lambda(Abs),
	is_value(Val),
	eval([Abs,Val],NewTerm),
	eval([NewTerm|OtherTerms],Result),
	% If second eval result is a value, you're done.
	is_value(Result),!.

eval([Abs,Val|OtherTerms],Result) :-
	is_lambda(Abs),
	is_value(Val),
	eval([Abs,Val],NewTerm),
	eval([NewTerm|OtherTerms],MidResult),
	% If second eval result not a value, continue eval.
	eval(MidResult,Result),!.

% E-APP2
eval([Val,Term], Result) :-
	is_value(Val),
	eval(Term,NewTerm),
	eval([Val,NewTerm], Result).

% E-APP1
eval([Term1,Term2], Result) :-
	eval(Term1,New1),
	eval([New1,Term2], Result).
