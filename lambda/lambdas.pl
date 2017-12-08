/* --- is_lambda check --- */
% Untyped Lambda Calculus
is_lambda(lam(X,_)) :-  % To be an abstraction you must have
	atom(X),!.			% unbound variable X.
% Simply Typed Lambda Calculus
is_lambda(lam(X:T,SubTerms)) :-
	type(T), is_lambda(lam(X,SubTerms)).

/* --- application evaluation --- */
% Untyped Lambda Calculus
apply(lam(X,SubTerms),Y,Result) :-	% Performing an application requires that
	atom(X),
	is_lambda(lam(X,SubTerms)), 	% The first term be a valid abstraction
	substitute(X, Y, SubTerms, Result). % We perform substitution to get result.
% Simply Typed Lambda Calculus
apply(lam(X:T,SubTerms),Y,Result) :-
	type(T),
	apply(lam(X,SubTerms),Y,Result),!.

/* --- environment list generation for mapping --- */
env_list_len(_,[],0).
env_list_len(Env,[Env|Tail],Len) :-
	LenMin is Len-1,
	env_list_len(Env, Tail, LenMin).

/* --- Variable Substitution --- */
% Used in version of abstraction and application which does not use Prolog
% variable for lambda calculus variables.
% To be read "NewTerm is the result of replacing all instances of Var in Term
%	with Val."
substitute(Var,Val,Var,Val) :- !.
substitute(Var, Val, TermList, NewList) :-
	is_list(TermList),
	maplist(substitute(Var,Val), TermList, NewList),!.
substitute(Var,Val,Term,Newterm) :-
    Term =.. [F|As], maplist(substitute(Var,Val),As,Rs), Newterm =.. [F|Rs], !.
substitute(_,_,U,U).

% Replace apply.
% apply(lam(X,SubTerm), Term, Result) :-
% 	replace(X,Term,SubTerm,Result).
