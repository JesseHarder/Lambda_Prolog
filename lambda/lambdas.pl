/* --- is_lambda check --- */
% Untyped Lambda Calculus
is_lambda(lam(X,SubTerms)) :-	% To be an abstraction you must have
	var(X),						% unbound variable X and
	is_list(SubTerms),!.		% a list of subterms, possibly containing X.
% Simply Typed Lambda Calculus
is_lambda(lam(X:T,SubTerms)) :-
	type(T), is_lambda(lam(X,SubTerms)).

/* --- application evaluation --- */
% Untyped Lambda Calculus
apply(lam(X,[SubTerm]),Y,Result) :-	% Performing an application requires that
	var(X),
	is_lambda(lam(X,[SubTerm])), 	% the first term be a valid abstraction
	X = Y,							% the unbound variable be equal the second term.
	Result = SubTerm,!.
apply(lam(X,SubTerms),Y,Result) :-	% Performing an application requires that
	var(X),
	is_lambda(lam(X,SubTerms)), 	% the first term be a valid abstraction
	X = Y,							% the unbound variable be equal the second term.
	Result = SubTerms,!.
% Simply Typed Lambda Calculus
apply(lam(X:T,SubTerms),Y,Result) :-
	type(T), apply(lam(X,SubTerms),Y,Result).

/* --- environment list generation for mapping --- */
env_list_len(_,[],0).
env_list_len(Env,[Env|Tail],Len) :-
	LenMin is Len-1,
	env_list_len(Env, Tail, LenMin).
