/* For implementing the small-step version of evaluation. Just for fun.
 */

:- [values,
	lambda/lambdas, lambda/rec_var,
	util/plists].

/****** eval/2 Big-Step Full Evaluation ******/
eval(Val, Val) :- is_value(Val).
eval(Term, Val) :-
	eval_ss(Term, TermPrime),
	eval(TermPrime, Val).

/****** eval/2 Big-Step Full Evaluation ******/

/* --- Booelean Evaluation --- */
%E-IfTrue
eval_ss(ifte(tru, Term2, _), Term2) :- !.
eval_ss(ifte(fls, _, Term3), Term3) :- !.
%E-If
eval_ss(ifte(Term1, Term2, Term3), ifte(New1, Term2, Term3)) :-
	eval_ss(Term1, New1),!.


/* --- Natural Number Evaluation --- */
% E-PredZero
eval_ss(pred(0), 0) :- !.
% E-PredSucc
eval_ss(pred(succ(Term)), Term) :-
	is_natural_value(Term),!.
% E-Succ
eval_ss(succ(Term),succ(NewTerm)) :-
	eval_ss(Term, NewTerm),!.
% E-Pred
eval_ss(pred(Term), pred(NewTerm)) :-
	eval_ss(Term, NewTerm),!.
% E-IsZeroZero
eval_ss(iszero(0), tru) :- !.
% E-IsZeroSucc
eval_ss(iszero(succ(X)), fls) :-
	is_natural_value(X),!.
% E-IsZero
eval_ss(iszero(Term), iszero(NewTerm)) :-
	eval_ss(Term, NewTerm),!.


/* --- Sequences with Unit type ---*/
% Sequence using lambda calculus.
eval_ss(seq([Term1, Term2]), Result) :-
	apply(lam(_:'Unit', Term2), Term1, Result),!.
eval_ss(seq([Term1, Term2 | OtherTerms]), seq([NewTerm | OtherTerms])) :-
	length([Term1, Term2 | OtherTerms], Len), Len > 2,
	eval_ss(seq([Term1, Term2]), NewTerm),!.
% The non-lambda, way.
% eval_ss(seq([Term]), Result) :-
% 	eval_ss(Term, Result),!.
% eval_ss(seq([FirstTerm|OtherTerms]), Result) :-
% 	eval_ss(FirstTerm, _),
% 	eval_ss(seq(OtherTerms), Result),!.


/* --- Let --- */
% let X=Term1 in Term2
% E-LetV
eval_ss(let(X=Val, Term2), Term2) :-
	var(X), is_value(Val),
	X=Val,!.
% E-Let
eval_ss(let(X=Term1, Term2), let(X=New1, Term2)) :-
	var(X), is_not_value(Term1),
	eval_ss(Term1, New1),!.


/* --- Tuples --- */
% E-ProjTuple
eval_ss(proj(tuple(List), Index), Result) :-
	is_value(tuple(List)),
	% Check that List is a non-empty list.
	is_list(List), length(List, Len), Index >= 1, Index =< Len,
	ith_elm(Index, List, Result),!.
% E-Proj
eval_ss(proj(tuple(List), Index), proj(NewTuple, Index)) :-
	eval_ss(tuple(List), NewTuple),!.
% E-Tuple - This is sort of the Big-Step version of this.
eval_ss(tuple(List), tuple(NewList)) :-
	is_not_value(tuple(List)),
	eval_first_non_value(List, NewList).


/* --- Records --- */
% E-ProjRecord
eval_ss(proj(record(List), Label),Result) :-
	is_value(record(List)),
	is_list(List), string(Label), % Sanity Check
	member(Label=Result, List),!.
% E-Proj
eval_ss(proj(record(List), Label), Result) :-
	is_not_value(record(List)),	% Sanity check.
	eval_ss(record(List), NewRecord),
	eval_ss(proj(NewRecord, Label), Result),!.
% E-Rcd
eval_ss(record(List), record(NewList)) :-
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
eval_ss(case(var(Label=Val), Conditions), Result) :-
	string(Label),	% Sanity check.
	is_value(Val),
	member(var(Label=Val)->CondTerm, Conditions),
	eval_ss(CondTerm, Result),!.
% E-Case
eval_ss(case(var(Label=Term), Conditions), Result) :-
	eval_ss(var(Label=Term), NewLabelTerm),
	eval_ss(case(NewLabelTerm, Conditions), Result),!.
% E-Variant
% The Small Step version.
% eval_ss(var(Label=Term), Result) :-
% 	(is_value(Term) ->
% 		% If Term is a value, var(Label=Term) is the result.
% 		Result = var(Label=Term);
% 		% If not, do another level of evaluation.
% 		eval_ss(Term, NewTerm),
% 		eval_ss(var(Label=NewTerm),Result)),!.
% The Big Step Version.
eval_ss(var(Label=Term), var(Label=Val)) :-
	eval_ss(Term,Val),!.


/* --- Lists --- */
% E-Cons1
eval_ss(cons(Term1, Term2), Result) :-
	eval_ss(Term1, New1),
	eval_ss(cons(New1, Term2), Result),!.
% E-Cons2
eval_ss(cons(Val1, Term2), Result) :-
	is_value(Val1),
	eval_ss(Term2, New2),
	eval_ss(cons(Val1, New2), Result),!.
% E-IsNilNil
eval_ss(isnil(nil), tru).
% E-IsNilCons
eval_ss(isnil(cons(V1, V2)), fls) :-
	is_value(V1),
	is_value(V2),!.
% E-IsNil
eval_ss(isnil(Term), Result) :-
	eval_ss(Term, NewTerm),
	eval_ss(isnil(NewTerm), Result),!.
% E-HeadCons
eval_ss(head(cons(V1, V2)), V1) :-
	is_value(V1),
	is_value(V2),!.
% E-Head
eval_ss(head(Term), Result) :-
	eval_ss(Term, NewTerm),
	eval_ss(head(NewTerm), Result),!.
% E-TailCons
eval_ss(tail(cons(V1, V2)), V2) :-
	is_value(V1),
	is_value(V2),!.
% E-Tail
eval_ss(tail(Term), Result) :-
	eval_ss(Term, NewTerm),
	eval_ss(tail(NewTerm), Result),!.


/* --- Exceptions --- */
/* - Errors w/o values - */
% E-AppError1
eval_ss([error, _], error).
% E-AppError2
eval_ss([_, error], error).
/* - Error Handling - */
% E-TryV
eval_ss(try(Val1, _), Val1) :-
	is_value(Val1),!.
% E-TryError
eval_ss(try(error, Term2), Result) :-
	 eval_ss(Term2, Result),!.
% E-Try
eval_ss(try(Term1, Term2), Result) :-
	 eval_ss(Term1, New1),
	 eval_ss(try(New1, Term2), Result),!.
/* - Raising Exceptions - */
% E-AppRaise1
eval_ss([raise(Val1), _], raise(Val1)) :-
	is_value(Val1),!.
% E-AppRaise2
eval_ss([Val1, raise(Val2)], raise(Val2)) :-
	is_value(Val1),
	is_value(Val2),!.
% E-Raise - Sort of Big Step version.
eval_ss(raise(Term), Result) :-
	eval_ss(Term, NewTerm),
	(is_value(NewTerm) ->
		Result = raise(NewTerm);
		eval_ss(raise(NewTerm), Result)),!.
% E-RaiseRaise
eval_ss(raise(raise(Val)), raise(Val)) :- is_value(Val),!.
% E-TryRaise
eval_ss(try(raise(Val), TryTerm), Result) :-
	is_value(Val),
	eval_ss([TryTerm, Val], Result),!.

/* --- Basic Lambda Calculus Evaluation --- */
% E-APP1
eval_ss([Term1, Term2], [New1, Term2]) :-
	eval_ss(Term1, New1),!.

% E-APP2
eval_ss([Val, Term2], [Val, New2]) :-
	eval_ss(Term2, New2),!.

% E-AppAbs
eval_ss([Abs, Val], Result) :-
	is_lambda(Abs),
	is_value(Val),
	apply(Abs, Val, Result),!.

eval_ss([Abs, Val|OtherTerms], [NewTerm|OtherTerms]) :-
	length([Abs, Val|OtherTerms], Len), Len > 2,
	eval_ss([Abs, Val], NewTerm),!.
