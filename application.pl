/*
 * Prolog code to be used to simulate lambda calculus application.
 *
 * The basic idea is that every lambda clalculus feature predicate Included
 * needs to have one or more apply() rules written for it, just as with the
 * typing rules. In general, these will look like the example below.
 *
 * --- Example ---
 * Say we have a predicate that simulates a lambda function wanting two
 * input variables. We need one apply rule for 1 variable and 2 vairables
 * and another for after the first has been applied.
 *
 * % Rule for applying both inputs.
 * apply(predicate(X,Y,Z), Input1, Input2, Result) :-
 *		var(X), % to check that X is the first variable.
 *		apply(predicate(Input1, Y, Z), Input2, Result).
 *
 * % Rule for applying the second input.
 * apply(predicate(_,Y,Z), Input2, Result) :-
 *		var(Y), % to check that Y is unbound variable.
 *		Result = predicate(Input1, Input2, Z),
 *		predicate(Input1, Input2, Z). 	% Note: This last line should only
 *										be included if application should
 *										also perform an evaluation.
 */

 :- [plists,
     numbers, booleans, tuples].
