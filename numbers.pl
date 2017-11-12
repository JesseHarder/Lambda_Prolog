/* Math */
% Binary
succ(X,Y) :- Y is X+1.
pred(X,Y) :- Y is X-1.
iszero(0,tru) :- !.
iszero(X,fls) :- number(X).

% Ternary
add(X,Y,S) :- S is X+Y.
sub(X,Y,D) :- D is X-Y.
mul(X,Y,P) :- P is X*Y.
div(N,D,Q) :- Q is N/D.
