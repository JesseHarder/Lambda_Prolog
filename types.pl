/*
 * Prolog code to be used to write type-checkable prolog code.
 *
 * Note, existing type checking predicates in prolog are listed here:
 * http://pauillac.inria.fr/~haemmerl/gprolog-rh/doc/manual024.html
 *
 */

 /* How to declare predicate types:
  *
  * type(Predicate_Name, Predicate_Arity, Type).
  *
  * where Predicate_Name and Predicate_Arity correspond to the second and third
  * terms in the functor/3 predicate,
  * and Type is a list of the expected types for the Terms of the functor.
  */

/* Predicate: type(Term, Type)
 * Meaning: Term "Term" has type "Type".
 */

type(X, number) :- number(X).
type(X, int) :- integer(X).
type(X, float) :- float(X).

 /* Predicate: type(Term, Type)
  * Meaning: Term "Term" has type "Type".
  * This is determined by checking for a predefined type with the name and
  * arity of the given predicate.
  */

 % type(Predicate, Type) :-
 %     functor(Predicate, Name, Arity),
 %     type(Name, Arity, Type).
