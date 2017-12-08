/* --- is_lambda check --- */
% Untyped Lambda Calculus
is_lambda(lam(X,_)) :-	% To be an abstraction you must have
	var(X),!.					% unbound variable X.
% Simply Typed Lambda Calculus
is_lambda(lam(X:T,SubTerms)) :-
	type(T), is_lambda(lam(X,SubTerms)).

/* --- application evaluation --- */
% Untyped Lambda Calculus
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

/* --- Variable Substitution --- */
% Used in version of abstraction and application which does not use Prolog
% variable for lambda calculus variables.
% To be read "NewTerm is the result of replacing all instances of Var in Term
%	with Val."
substitutue(Var,Val,Term,NewTerm) :-
	Term=Var, NewTerm=Val,!.
substitutue(Var,Val,Term,Newterm) :-
    Term =.. [F|As], maplist(substitutue(Var,Val),As,Rs), Newterm =.. [F|Rs], !.
substitutue(_,_,U,U).

% Replace apply.
% apply(lam(X,SubTerm), Term, Result) :-
% 	replace(X,Term,SubTerm,Result).
