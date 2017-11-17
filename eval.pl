/* This file contains all of the lambda calculus evaluation rules as
 * implemented in Prolog. The predicate evaluate/2 is used for these
 * rules. They take the form:
 * evaluate(X,Y)
 *		where Y is the result of evaulating X.
 */

:- [values,
	lambda/lambdas].

/* --- Booelean Evaluation --- */

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
