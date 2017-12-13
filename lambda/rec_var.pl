/* Utility functions for working with records and variants.
 * where the elements of List are Terms.
 * The representation of records and variants is:
 * 	record(List) and variant(List) 		(respectively)
 * where the elements in List are of the form
 *	Label=Term
 * where Label is a string and Term is some term in our language.
 */

% Splits/merges list elements surrounding "=" into two lists.
list_split_eq([], [], []).
list_split_eq([Label=Term], [Label], [Term]).
list_split_eq([Label=Term|Pairs], [Label|Labels], [Term|Terms]) :-
	list_split_eq(Pairs,Labels,Terms).

% Record list splitting.
% The first term is a valid record.
% The second term is a list of the labels in the record in the order they appear.
% The third term is a list of the terms in the record in the order they appear.
record_parts(record(List), Labels, Terms) :- list_split_eq(List,Labels,Terms).

% Variant list splitting.
% The first term is a valid variant type.
% The second term is a list of the labels in the variant in the order they appear.
% The third term is a list of the terms in the variant in the order they appear.
variant_parts('Variant'(List), Labels, Terms) :- list_split_eq(List,Labels,Terms).
