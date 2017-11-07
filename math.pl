[types].

add(X,Y,S) :- S is X+Y.
% sub(X,Y,D) :- D is X-Y.
% mult(X,Y,P) :- P is X*Y.

type(sum(_,_,_),[int,int,int]).
