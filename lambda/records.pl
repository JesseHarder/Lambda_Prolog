/* Utility functions for working with records. The representation of records is:
 * 	record(List)
 * where the elements in List are of the form
 *	Label=Term
 * where Label is a string and Term is some term in our language.
 */

% Record list splitting.
% The first term is a valid record.
% The second term is a list of the labels in the record in the order they appear.
% The third term is a list of the terms in the record in the order they appear.
record_parts(record([]), [], []).
record_parts(record([Label=Term]), [Label], [Term]).
record_parts(record([Label=Term|Pairs]), [Label|Labels], [Term|Terms]) :-
	record_parts(record(Pairs),Labels,Terms).
