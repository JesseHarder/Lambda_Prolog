/*
 * Prolog code to be used to write type-checkable prolog code.
 *
 * Note, existing type checking predicates in prolog are listed here:
 * http://pauillac.inria.fr/~haemmerl/gprolog-rh/doc/manual024.html
 *
 */

:- dynamic type/1.
:- dynamic type/2.

/* Variables - can be of any type. */
% 		This is the main thing all files need to know about typing.
type(Var, Type) :- var(Var), type(Type).
