/* This file contains helper functions for working with Prolog Lists.
 */

/* Function to restructure lists as follows:
 *	[a,b,c,d,e] becomes [[[[a,b],c],d],e]
 * First value should be list of the form on the left above.
 * Second value should be list of the form on the right above.
 */
list_layer_left([A,B], [A,B]).
list_layer_left([A,B|Tail], Result) :-
    list_layer_left([[A,B]|Tail], Result).
