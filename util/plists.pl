/* This file contains helper functions for working with PROLOG Lists.
 * This file does not contain anything relating to Lambda Calc lists
 * in any way other than that they share a representation.
 */

/* Function for getting the ith element of a list. */
ith_elm([Head|_], 1, Head).
ith_elm([_|Tail], I, Elm) :-
	I > 1, J is I-1,
	ith_elm(Tail, J, Elm).
