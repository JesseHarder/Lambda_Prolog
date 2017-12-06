/* Utility functions for working with tuples. The representation of records is:
 * 	tuple(List)
 * where the elements in List are Terms.
 */

eval_first_non_value(tuple(List), tuple(NewList)) :-
