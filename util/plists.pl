/* This file contains helper functions for working with PROLOG Lists.
 * This file does not contain anything relating to Lambda Calc lists
 * in any way other than that they share a representation.
 */

/* Function for getting the ith element of a list. */
ith_elm([Head|_], 1, Head).
ith_elm([_|Tail], I, Elm) :-
	I > 1, J is I-1,
	ith_elm(Tail, J, Elm).

/* Function to restructure lists as follows:
 *	[a,b,c,d,e] becomes [[[[a,b],c],d],e]
 * First value should be list of the form on the left above.
 * Second value should be list of the form on the right above.
 */
list_layer_left([A,B], [A,B]).
list_layer_left([A,B|Tail], Result) :-
    list_layer_left([[A,B]|Tail], Result).
