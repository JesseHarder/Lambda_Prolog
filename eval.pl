/* This file contains all of the lambda calculus evaluation rules as
 * implemented in Prolog. The predicate evaluate/2 is used for these
 * rules. They take the form:
 * evaluate(X,Y)
 *		where Y is the result of evaulating X.
 */

:- [lambda/lambdas].

/* Basic Lambda Calculus Evaluation*/
% Variables - evalute to themselves. (This might cause infinite loop.)
eval(X,X) :- atom(X).

% Application Evaluation
eval([Term1,Term2],Result) :-
	apply(Term1,Term2,Result),!.

eval([Term1,Term2|OtherTerms],Result) :-
	apply(Term1,Term2,NewTerm),
	eval([NewTerm|OtherTerms],Result).
