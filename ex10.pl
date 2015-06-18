% use is instead of = to assign to a var the value of a numerical expression
len([], 0).
len([_|T], N) :- len(T, N1), N is N1 + 1.

% map

map(_, [], []).
map(F, [X|XS], [Y|YS]) :- call(F, X, Y), map(F, XS, YS).

double(N, R) :- R is N * N.

% filter version 1

filter(_, [], []).
filter(P, [X|XS], [X|YS]) :- call(P, X), filter(P, XS, YS).
filter(P, [X|XS], YS) :- \+ call(P, X), filter(P, XS, YS).

even(N) :- 0 is N mod 2.
odd(N) :- \+ even(N).

% better version of filter using the cut operator (!)
filter2(_, [], []).
filter2(P, [X|XS], [X|YS]) :- call(P, X), !, filter2(P, XS, YS).
filter2(P, [_|XS], YS) :- filter2(P, XS, YS).

% tozero returns all the numbers from a positive number to zero
tozero(0, 0) :- !.
tozero(Y, Z) :- Z = Y ; Y1 is Y-1, tozero(Y1, Z). 

% find with cut
find([X|_], X) :- !.
find([_|Y], X) :- find(Y, X).

% find2 is equivalent to find, keep attention to the use of = instead of == (try to query find2 with an undefined second parameter e.g., find2([1,2,3], X))
find2([Y|YS], X) :- Y=X, ! ; find2(YS, X).

% fold left

foldl(_, [], Acc, Acc).
foldl(F, [X|XS], Acc, R) :- call(F, X, Acc, Y), foldl(F, XS, Y, R).

% emits all possible right triangles that you can create 
right_triangles(N, A, B, C) :- between(1, N, C), between(1, C, B), between(1, B, A), C^2 =:= A^2 + B^2.

% reverse + map

revmap(_, [], []).
revmap(F, [X|XS], Z) :- revmap(F, XS, Y), call(F, X, T), append(Y, [T], Z).

revmap(_, [], Acc, Acc).
revmap(F, [X|XS], Acc, Z) :- call(F, X, T), revmap(F, XS, [T|Acc], Z).

% prolog cant unify a(b,c) with something like X(Y, Z). if you need to decompose a term you can you the =.. operator. 
% =.. binds a term with a list containg the term name (functor) and its arguments e.g., a(b, c) =.. X, X will be [a, b, c].

pred(X) :- X =.. [P|Args], write("Predicate '"), write(P), write("' with args: "), writeln(Args).

% derivator

d(U+V, X, DU+DV) :- !, d(U, X, DU), d(V, X, DV). 
d(U-V, X, DU-DV) :- !, d(U, X, DU), d(V, X, DV). 
d(U*V, X, DU*V+DV*U) :- !, d(U, X, DU), d(V, X, DV). 
d(U^N, X, N*U^N1*DU) :- !, integer(N), N1 is N-1, d(U, X, DU). 
d(-U, X, -DU) :- !, d(U, X, DU).

d(X,X,1) :- !.
d(C, _, 0) :- atomic(C), !.
d(sin(X), X, cos(X)) :- !.
d(cos(X), X, -sin(X)) :- !.
d(exp(X), X, exp(X)) :- !.
d(log(X), X, 1/X) :- !.

d(F_G, X, DF*DG) :- F_G =.. [_,G], !, d(F_G, G, DF), d(G, X, DG).

% try this: d(log(x^2+x), x, Z).
