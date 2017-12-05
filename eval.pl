/* This file contains all of the lambda calculus evaluation rules as
 * implemented in Prolog. The predicate evaluate/2 is used for these
 * rules. They take the form:
 * eval(X,Y)
 *		where Y is the result of evaulating X.
 */

:- [values,
	lambda/lambdas, lambda/rec_var,
	util/plists].


% eval_if_not_value/2
% If the term to be evaluated is already a value, the result is the term.
% If the term to be evaluated is not a value, the result is the evaluation of
%	the term.
eval_if_not_value(Term, Result) :- eval(Term, Result).
	% (is_value(Term) ->
	% 	% If Term is a value, it is the result.
	% 	Result = Term;
	% 	% If not, the result of evaluating it is the result.
	% 	eval(Term, Result)),!.

/* --- Value --- */
eval(Val, Val) :- is_value(Val).

/* --- Booelean Evaluation --- */
%E-IfTrue
eval(ifte(tru, Term2, _) ,Result) :-
	eval_if_not_value(Term2,Result),!.
%E-IfFalse
eval(ifte(fls, _, Term3), Result) :-
	eval_if_not_value(Term3, Result),!.
%E-If
eval(ifte(Term1, Term2, Term3), Result) :-
	eval(Term1, New1),
	eval_if_not_value(ifte(New1, Term2, Term3), Result),!.


/* --- Natural Number Evaluation --- */
% E-PredZero
eval(pred(0), 0) :- !.
% E-PredSucc
eval(pred(succ(Term)), Term) :-
	is_natural_value(Term),!.
% E-Succ
eval(succ(Term), Result) :-
	eval(Term, NewTerm),
	eval_if_not_value(succ(NewTerm), Result),!.
% E-Pred
eval(pred(Term), Result) :-
	eval(Term, NewTerm),
	eval(pred(NewTerm), Result),!.
% E-IsZeroZero
eval(iszero(0), tru) :- !.
% E-IsZeroSucc
eval(iszero(succ(X)), fls) :-
	is_natural_value(X),!.
% E-IsZero
eval(iszero(Term), Result) :-
	eval(Term, NewTerm),
	eval(iszero(NewTerm),Result),!.


/* --- Sequences with Unit type ---*/
% Sequence using lambda calculus.
eval(seq([Term1, Term2]), Result) :-
	apply(lam(_:'Unit', [Term2]), Term1, MidResult),
	eval_if_not_value(MidResult, Result),!.
eval(seq([Term1, Term2 | OtherTerms]), Result) :-
	length([Term1, Term2 | OtherTerms], Len), Len > 2,
	eval(seq([Term1, Term2]), MidResult),
	eval(seq([MidResult | OtherTerms]), Result),!.
% The non-lambda, way.
% eval(seq([Term]), Result) :-
% 	eval(Term, Result),!.
% eval(seq([FirstTerm|OtherTerms]), Result) :-
% 	eval(FirstTerm, _),
% 	eval(seq(OtherTerms), Result),!.


/* --- Let --- */
% let X=Term1 in Term2
% E-LetV
eval(let(X=Val, Term2), Result) :-
	var(X), is_value(Val),
	X=Val,
	eval(Term2, Result),!.
% E-Let
eval(let(X=Term1, Term2), Result) :-
	var(X), is_not_value(Term1),
	eval(Term1, New1),
	eval(let(X=New1, Term2), Result),!.


/* --- Tuples --- */
% E-ProjTuple
eval(proj(tuple(List), Index), Result) :-
	is_value(tuple(List)),
	% Check that List is a non-empty list.
	is_list(List), length(List, Len), Index >= 1, Index =< Len,
	ith_elm(Index, List, Result),!.
% E-Proj
eval(proj(tuple(List), Index), Result) :-
	eval(tuple(List), NewTuple),
	eval(proj(NewTuple, Index), Result),!.
% E-Tuple - This is sort of the Big-Step version of this.
eval(tuple(List), tuple(Vals)) :-
	is_not_value(tuple(List)),
	maplist(eval_if_not_value, List, Vals),!.


/* --- Records --- */
% E-ProjRecord
eval(proj(record(List), Label),Result) :-
	is_value(record(List)),
	is_list(List), string(Label), % Sanity Check
	member(Label=Result, List),!.
% E-Proj
eval(proj(record(List), Label), Result) :-
	is_not_value(record(List)),	% Sanity check.
	eval(record(List), NewRecord),
	eval(proj(NewRecord, Label), Result),!.
% E-Rcd
eval(record(List), record(NewList)) :-
	is_not_value(record(List)),
	record_parts(record(List), Labels, Terms),
	maplist(eval_if_not_value, Terms, Vals),
	record_parts(record(NewList), Labels, Vals),!.


/* --- Variant Types: Case ---
 *	Variant elements are represented as var(X=Y) where:
 * 		X is some string serving as the label.
 * 		Y is some term.
 *	The Conditions variable appearing within variant cases should be a list of
 *	elements of the form var(A=B)->C where:
 *		A is a string serving as the label.
 *		B is the variable appearing in C to be replaced.
 *		C is the term in which B will be replaced by the case statement.
 */
% E-CaseVariant
eval(case(var(Label=Val), Conditions), Result) :-
	string(Label),	% Sanity check.
	is_value(Val),
	member(var(Label=Val)->CondTerm, Conditions),
	eval_if_not_value(CondTerm, Result),!.
% E-Case
eval(case(var(Label=Term), Conditions), Result) :-
	eval(var(Label=Term), NewLabelTerm),
	eval(case(NewLabelTerm, Conditions), Result),!.
% E-Variant
% The Small Step version.
% eval(var(Label=Term), Result) :-
% 	(is_value(Term) ->
% 		% If Term is a value, var(Label=Term) is the result.
% 		Result = var(Label=Term);
% 		% If not, do another level of evaluation.
% 		eval(Term, NewTerm),
% 		eval(var(Label=NewTerm),Result)),!.
% The Big Step Version.
eval(var(Label=Term), var(Label=Val)) :-
	eval_if_not_value(Term,Val),!.


/* --- Lists --- */
% E-Cons1
eval(cons(Term1, Term2), Result) :-
	eval(Term1, New1),
	eval_if_not_value(cons(New1, Term2), Result),!.
% E-Cons2
eval(cons(Val1, Term2), Result) :-
	is_value(Val1),
	eval(Term2, New2),
	eval_if_not_value(cons(Val1, New2), Result),!.
% E-IsNilNil
eval(isnil(nil), tru).
% E-IsNilCons
eval(isnil(cons(V1, V2)), fls) :-
	is_value(V1),
	is_value(V2),!.
% E-IsNil
eval(isnil(Term), Result) :-
	eval(Term, NewTerm),
	eval_if_not_value(isnil(NewTerm), Result),!.
% E-HeadCons
eval(head(cons(V1, V2)), V1) :-
	is_value(V1),
	is_value(V2),!.
% E-Head
eval(head(Term), Result) :-
	eval(Term, NewTerm),
	eval(head(NewTerm), Result),!.
% E-TailCons
eval(tail(cons(V1, V2)), V2) :-
	is_value(V1),
	is_value(V2),!.
% E-Tail
eval(tail(Term), Result) :-
	eval(Term, NewTerm),
	eval(tail(NewTerm), Result),!.


/* --- Exceptions --- */
/* - Errors w/o values - */
% E-AppError1
eval([error, _], error).
% E-AppError2
eval([_, error], error).
/* - Error Handling - */
% E-TryV
eval(try(Val1, _), Val1) :-
	is_value(Val1),!.
% E-TryError
eval(try(error, Term2), Result) :-
	 eval_if_not_value(Term2, Result),!.
% E-Try
eval(try(Term1, Term2), Result) :-
	 eval(Term1, New1),
	 eval(try(New1, Term2), Result),!.
/* - Raising Exceptions - */
% E-AppRaise1
eval([raise(Val1), _], raise(Val1)) :-
	is_value(Val1),!.
% E-AppRaise2
eval([Val1, raise(Val2)], raise(Val2)) :-
	is_value(Val1),
	is_value(Val2),!.
% E-Raise - Sort of Big Step version.
eval(raise(Term), Result) :-
	eval(Term, NewTerm),
	(is_value(NewTerm) ->
		Result = raise(NewTerm);
		eval(raise(NewTerm), Result)),!.
% E-RaiseRaise
eval(raise(raise(Val)), raise(Val)) :- is_value(Val),!.
% E-TryRaise
eval(try(raise(Val), TryTerm), Result) :-
	is_value(Val),
	eval([TryTerm, Val], Result),!.

/* --- Basic Lambda Calculus Evaluation --- */
% E-APP1
eval([Term1, Term2], Result) :-
	is_not_value(Term1),
	% is_not_value(Term2),
	eval(Term1, New1),
	eval([New1, Term2], Result),!.

% E-APP2
eval([Val, Term2], Result) :-
	is_value(Val),
	is_not_value(Term2),
	eval(Term2, New2),
	eval([Val, New2], Result),!.

% E-AppAbs
eval([Abs, Val],Result) :-
	is_lambda(Abs),
	is_value(Val),
	apply(Abs, Val, ApResult),
	eval_if_not_value(ApResult, Result),!.

eval([Abs, Val|OtherTerms], Result) :-
	length([Abs, Val|OtherTerms], Len), Len > 2,
	eval([Abs, Val], NewTerm),
	eval([NewTerm|OtherTerms], MidResult),
	eval_if_not_value(MidResult, Result),!.
