% Lambdas
is_lambda(lam(X,SubTerms)) :-	% To be an abstraction you must have
	var(X),					% unbound variable X and
	is_list(SubTerms).		% a list of subterms, possibly containing X.

apply(lam(X,[SubTerm]),Y,Result) :-	% Performing an application requires that
	is_lambda(lam(X,[SubTerm])), 	% the first term be a valid abstraction
	X = Y,						% the unbound variable be equal the second term.
	Result = SubTerm,!.
apply(lam(X,SubTerms),Y,Result) :-	% Performing an application requires that
	is_lambda(lam(X,SubTerms)), 	% the first term be a valid abstraction
	X = Y,						% the unbound variable be equal the second term.
	Result = SubTerms.
